module Language.Typo.Compiler.Expression
  ( anf
  , redex
  , value
  , number
  , operation
  , operationLookup
  , mkName
  , mkId
  , mkTv
  ) where

import Language.Typo.ASTs

import Bag ( unitBag )
import HsBinds
import HsSyn
import SrcLoc ( noLoc )
import RdrName
import OccName


anf :: ANF -> LHsExpr RdrName
anf e =
  case e of
    ARed r -> redex r
    ALet (SingleBind x r) b ->
      let x' = mkName x
          r' = redex r
          b' = anf b
          binds = HsValBinds (ValBindsIn (unitBag (noLoc (VarBind x' r' False))) [])
        in noLoc (HsLet binds b')

redex :: Redex -> LHsExpr RdrName
redex r =
  case r of
    RVal v -> value v
    RApp f as ->
      let f'  = noLoc (mkId f)
          as' = map value as
        in mkApp f' as'
    RBop op l r ->
      let op' = operation op
          l'  = value l
          r'  = value r
        in mkApp op' [l', r']
    RCond c t f ->
      let c' = value c
          t' = anf t
          f' = anf f
          cond = noLoc (mkId "cond")
        in mkApp cond [c', t', f']

value :: Value -> LHsExpr RdrName
value v = noLoc $
  case v of
    Number w ->
      ExprWithTySig
        (noLoc (mkId "undefined"))
        (number w)
    Boolean b -> mkId $
      if b then "true"
           else "false"
    Id s -> mkId s

-- XXX: The `fromIntegral` is not good, but probably won't come up in practice.
--      If it does, that right there is what's causing the problem.
number :: Integral a => a -> LHsType RdrName
number n = (iterate (\t -> noLoc (HsAppTy s t)) z) !! (fromIntegral n)
  where
    s = noLoc (mkTv "S")
    z = noLoc (mkTv "Z")

operation :: Op -> LHsExpr RdrName
operation = noLoc . mkId . operationLookup

operationLookup :: Op -> String
operationLookup op =
  case op of
    Add -> "add"
    Sub -> "sub"
    Mul -> "mul"
    Div -> "div"
    Rem -> "rem"
    And -> "and"
    Or  -> "or"
    Imp -> "imp"
    Eq  -> "eq"
    Lt  -> "lt"

mkName = mkRdrUnqual . mkVarOcc
mkId = HsVar . mkName
mkTv = HsTyVar . mkRdrUnqual . mkTyVarOcc
mkApp = foldl (\a b -> noLoc (HsApp a b))
