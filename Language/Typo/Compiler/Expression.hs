module Language.Typo.Compiler.Expression
  ( anf
  , redex
  , value
  , number
  , operation
  , operationLookup
  ) where

import Language.Typo.ASTs

import Language.Haskell.TH.Syntax


anf :: ANF -> Exp
anf e =
  case e of
    ARed r -> redex r
    ALet x r b ->
      let x' = mkName x
          r' = redex r
          b' = anf b
          bind = ValD (VarP x') (NormalB r') []
        in LetE [bind] b'

redex :: Redex -> Exp
redex r =
  case r of
    RVal v -> value v
    RApp f as ->
      let f'  = VarE (mkName f)
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
          cond = VarE (mkName "cond")
        in mkApp cond [c', t', f']

value :: Value -> Exp
value v =
  case v of
    Number w ->
      SigE (VarE (mkName "undefined")) (number w)
    Boolean b -> VarE $ mkName $
      if b then "true"
           else "false"
    Id s -> VarE $ mkName s

-- XXX: The `fromIntegral` is not good, but probably won't come up in practice.
--      If it does, that right there is what's causing the problem.
number :: Integral a => a -> Type
number n = (iterate (\t -> AppT s t) z) !! (fromIntegral n)
  where
    s = ConT (mkName "S")
    z = VarT (mkName "Z")

operation :: Op -> Exp
operation = VarE . mkName . operationLookup

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

mkApp = foldl AppE
