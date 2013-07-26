module Language.Typo.ASTs
  ( Value(..)
  , Op(..)
  , Surface(..)
  , Definition(..)
  , Program(..)
  ) where

import Data.Word


data Value
  = Number Word
  | Boolean Bool
  | Id String
  deriving ( Eq, Ord, Show )

data Op
  = Add | Sub | Mul | Div | Rem
  | And | Or | Imp | Eq | Lt
  deriving ( Eq, Ord, Enum, Bounded, Show )

data Surface
  = Val Value
  | Let String Surface Surface
  | App String [Surface]
  | Bop Op Surface Surface
  | Cond Surface Surface Surface
  deriving ( Eq, Ord, Show )

data Definition a
  = Definition { name :: String, args :: [String], body :: a }
  deriving ( Eq, Ord, Show )

data Program a
  = Program { definitions :: [Definition a], expr :: a }
  deriving ( Eq, Ord, Show )
