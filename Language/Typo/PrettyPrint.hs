{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.Typo.PrettyPrint
  ( PrettyPrint(..)
  , render          -- :: Doc -> String
  , prettyRender    -- :: PrettyPrint a => a -> String
  ) where

import Data.Maybe ( fromJust )

import Text.PrettyPrint.HughesPJ

import Language.Typo.ASTs
import Language.Typo.Parser ( operatorTable )


prettyRender :: PrettyPrint a => a -> String
prettyRender = render . prettyPrint

class PrettyPrint a where
  prettyPrint :: a -> Doc

instance PrettyPrint Value where
  prettyPrint v =
    case v of
      Number i -> integer (fromIntegral i)
      Boolean b -> if b then text "#t" else text "#f"
      Id x -> text x

instance PrettyPrint Op where
  prettyPrint op = text (fromJust (lookup op (map swap operatorTable)))
    where swap (a, b) = (b, a)


instance PrettyPrint Surface where
  prettyPrint s =
    case s of
      Val v -> prettyPrint v
      Let (SingleBind x e) b -> _let x e b
      App f as -> _app f as
      Bop op l r -> _bop op l r
      Cond c t f -> _if c t f 

instance PrettyPrint Redex where
  prettyPrint r =
    case r of
      RVal v -> prettyPrint v
      RApp f as -> _app f as
      RBop op l r -> _bop op l r
      RCond c t f -> _if c t f

instance PrettyPrint ANF where
  prettyPrint e =
    case e of
      ARed r -> prettyPrint r
      ALet (SingleBind x r) b -> _let x r b

instance PrettyPrint a => PrettyPrint (Definition a) where
  prettyPrint d =
    let top = (text "def") <+> (parens ((text (name d)) <+> (hsep (map text (args d)))))
      in parens (hang top 2 (prettyPrint (body d)))

instance PrettyPrint a => PrettyPrint (Program a) where
  prettyPrint p = 
    vcat $ (map prettyPrint (definitions p)) ++ [prettyPrint (expr p)]


_app f as = parens (text f <+> (hsep (map prettyPrint as)))
_bop op l r = parens (prettyPrint op <+> (hsep (map prettyPrint [l, r])))
_if c t f = parens (hang (text "if" <+> (prettyPrint c)) 3 ((prettyPrint t) <+> (prettyPrint f)))
_let x e b =
  let top = (text "let") <+> (parens (text x <+> (prettyPrint e)))
    in parens (hang top 2 (prettyPrint b))
