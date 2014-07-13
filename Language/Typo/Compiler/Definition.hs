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

import Language.Haskell.TH.Syntax


definition :: Definition ANF -> [Dec]
definition (Definition name args expr) = 
  (mkClassDecl name args):(anf name args expr)

data Situation = Situation {
    env :: Env,
    context :: Cxt
}

type CM = ReaderT Situation (StateT Int (Writer [Dec]))


anf :: String -> [String] -> ANF -> [Dec]
anf name args e = snd (runWriter (evalStateT (runReaderT (anf' name args e) sit) 0))
  where
    sit = Situation env []
    env = foldl (\e a -> addEnv a (VarT (mkName a)) e) emptyEnv args

anf' :: String -> [String] -> ANF -> CM ()
anf' name args e =
  case e of
    ARed r -> do
      (res, cs) <- redex name r
      cs'   <- asks context
      args' <- mapM (\k -> asks (lookupEnv k . env)) args
      tell [mkClassInst (cs ++ cs') name args' res]
    ALet x r b -> do
      (res, cs) <- redex name r
      local (letSituation x res cs)
        (anf' name args b)
  where
    letSituation k v cs s = s {
        env = addEnv k v (env s),
        context = cs ++ (context s)
    }

redex :: String -> Redex -> CM (Type, Cxt)
redex name r =
  case r of
    RVal v -> do
      v' <- value v
      return (v', [])
    RApp f vs -> do
      result <- gentv "res"
      args   <- mapM value vs
      let cxt = ClassP (mkCName f) (args ++ [result])
      return (result, [cxt])
    RBop op l r -> do
      result <- gentv "res"
      args'  <- mapM value [l, r]
      let cxt = ClassP (operation op) (args' ++ [result])
      return (result, [cxt])
    RCond c t f -> do
      branch <- gensym (name ++ "Branch")
      result <- gentv "res"
      (ns, vs) <- asks (unzip . toListEnv . env)

      -- XXX: This passes the entire environment to the branch even if some
      -- identifiers aren't used in the branch.
      tell [mkClassDecl branch ns]

      let true = VarT (mkName "True")
          false = VarT (mkName "False")
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

      let cxt = ClassP (mkCName branch) (vs ++ [result])
      return (result, [cxt])
  where
    noContextSituation s = s {
      context = []
    }
    branchSituation k v s = s {
      env = addEnv k v (env s),
      context = []
    }

value :: Value -> CM Type
value v =
  case v of
    Number w -> return (E.number w)
    Boolean b -> return (VarT (mkName (if b then "True" else "False")))
    Id s -> asks ((lookupEnv s) . env)

operation :: Op -> Name
operation = mkCName . E.operationLookup

mkClassDecl name args =
  ClassD [] (mkCName name)
    (map PlainTV (args' ++ [resname]))
    [FunDep args' [resname]]
    [ SigD (mkName name)        (mkFunTy restype args)
    , ValD (VarP (mkName name)) (NormalB (VarE (mkName "undefined"))) []
    ]
  where
    restype = VarT (mkName "res")
    resname = mkName "res"
    args' = map mkName args

mkClassInst ctxts name args result =
  InstanceD ctxts (foldl AppT (ConT (mkCName name)) (args ++ [result])) []

mkFunTy :: Type -> [String] -> Type
mkFunTy r = (foldr (\a b -> AppT (AppT ArrowT a) b) r) . (map (VarT . mkName))

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (toUpper c):cs

mkCName = mkName . capitalize 

gentv = (fmap (VarT . mkName)) . gensym

type Env = Map String Type
emptyEnv = M.empty
addEnv = M.insert
toListEnv = M.toList
lookupEnv k env =
  case M.lookup k env of
    Just s -> s
    Nothing -> error $ "free variable found: " ++ k
