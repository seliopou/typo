module Language.Typo.Compiler.Definition
  ( definition
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Char
import Data.Map ( Map )
import qualified Data.Map as M

import Language.Typo.ASTs

import qualified Language.Typo.Compiler.Expression as E

import Bag ( emptyBag, unitBag )
import HsDecls
import HsSyn
import NameSet ( emptyNameSet )
import SrcLoc ( Located, noLoc )
import RdrName
import OccName


definition :: Definition ANF -> [Located (HsDecl RdrName)]
definition (Definition name args expr) = 
  (noLoc (TyClD (mkClassDecl name args))):(anf name args expr)

data Situation = Situation {
    env :: Env,
    context :: [LHsType RdrName]
}

type CM = ReaderT Situation (StateT Int (Writer [Located (HsDecl RdrName)]))


anf :: String -> [String] -> ANF -> [Located (HsDecl RdrName)]
anf name args e = snd (runWriter (evalStateT (runReaderT (anf' name args e) sit) 0))
  where
    sit = Situation env []
    env = foldl (\e a -> addEnv a (noLoc (E.mkTv a)) e) emptyEnv args

anf' :: String -> [String] -> ANF -> CM ()
anf' name args e =
  case e of
    ARed r -> do
      (res, cs) <- redex name r
      cs'   <- asks context
      args' <- mapM (\k -> asks (lookupEnv k . env)) args
      tell [noLoc (InstD (mkClassInst (cs ++ cs') name args' res))]
    ALet x r b -> do
      (res, cs) <- redex name r
      local (letSituation x res cs)
        (anf' name args b)
  where
    letSituation k v cs s = s {
        env = addEnv k v (env s),
        context = cs ++ (context s)
    }

redex :: String -> Redex -> CM (LHsType RdrName, [LHsType RdrName])
redex name r =
  case r of
    RVal v -> do
      v' <- value v
      return (v', [])
    RApp f vs -> do
      result <- gentv "res"
      args   <- mapM value vs
      let cxt = noLoc (mkHsAppTys (mkClassTv f) (args ++ [result]))
      return (result, [cxt])
    RBop op l r -> do
      result <- gentv "res"
      args'  <- mapM value [l, r]
      let cxt = noLoc (mkHsAppTys (operation op) (args' ++ [result]))
      return (result, [cxt])
    RCond c t f -> do
      branch <- gensym (name ++ "Branch")
      result <- gentv "res"
      (ns, vs) <- asks (unzip . toListEnv . env)

      -- XXX: This passes the entire environment to the branch even if some
      -- identifiers aren't used in the branch.
      tell [noLoc (TyClD (mkClassDecl branch ns))]

      let true = noLoc (E.mkTv "True")
          false = noLoc (E.mkTv "False")
      case c of
        Number _  ->
          -- Type error.
          error "literal number as a condition"
        Boolean b ->
          -- Kill dead code. This optimization also makes implementation
          -- simpler. In order to pass values to a branch, you have to name the
          -- values. Since literals don't have names, that would entail
          -- expanding the environment. Ain't nobody got time for that.
          local noContextSituation $
            if b then anf' branch ns t
                 else anf' branch ns f
        Id s -> do
          local (branchSituation s true)
            (anf' branch ns t)
          local (branchSituation s false)
            (anf' branch ns f)

      let cxt = noLoc (mkHsAppTys (mkClassTv branch) (vs ++ [result]))
      return (result, [cxt])
  where
    noContextSituation s = s {
      context = []
    }
    branchSituation k v s = s {
      env = addEnv k v (env s),
      context = []
    }

value :: Value -> CM (LHsType RdrName)
value v =
  case v of
    Number w -> return (E.number w)
    Boolean b -> return (noLoc (if b then E.mkTv "True" else E.mkTv "False"))
    Id s -> asks ((lookupEnv s) . env)

operation :: Op -> LHsType RdrName
operation = mkClassTv . E.operationLookup

mkClassName = mkRdrUnqual . mkClsOcc . capitalize
mkClassTv = noLoc . HsTyVar . mkClassName

mkClassDecl name args =
  ClassDecl
    (noLoc [])
    (noLoc (mkClassName name))
    (HsQTvs [] (map (noLoc . UserTyVar) (args' ++ [resname])))
    [noLoc (args', [resname])]
    [noLoc (TypeSig [noLoc dname] (mkFunTy restype args))]
    (unitBag (noLoc (VarBind dname undef False)))
    [] [] [] emptyNameSet
  where
    restype = noLoc (E.mkTv "res")
    resname = E.mkName "res"
    dname = E.mkName name
    undef = noLoc (E.mkId "undefined")
    args' = map E.mkName args

mkClassInst ctxts name args result = ClsInstD
  (noLoc 
    (mkImplicitHsForAllTy (noLoc ctxts)
      (noLoc (mkHsAppTys (mkClassTv name)
                         (args ++ [result])))))
  emptyBag
  []
  []

mkFunTy :: LHsType RdrName -> [String] -> LHsType RdrName
mkFunTy r = (foldr (\a b -> noLoc (HsFunTy a b)) r) . (map (noLoc . E.mkTv))

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (toUpper c):cs

gentv = (fmap (noLoc . E.mkTv)) . gensym

type Env = Map String (LHsType RdrName)
emptyEnv = M.empty
addEnv = M.insert
toListEnv = M.toList
lookupEnv k env =
  case M.lookup k env of
    Just s -> s
    Nothing -> error $ "free variable found: " ++ k
