module Language.Typo.ASTs
  ( Value(..)
  , Surface(..)
  , Op(..)
  ) where

import Data.Word


data Value
  = Number Word
  | Boolean Bool
  | Id String
  deriving ( Eq, Ord, Show )

data Op
  = Add | Sub | Mul | Div | Rem
  | And | Or | Xor | Eq | Lt
  deriving ( Eq, Ord, Enum, Bounded, Show )

data Surface
  = Val Value
  | Let String Surface Surface
  | App String [Surface]
  | Bop Op Surface Surface
  | Cond Surface Surface Surface
  deriving ( Eq, Ord, Show )
