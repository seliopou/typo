module Language.Typo.Compiler
  ( compile     -- :: Program Surface -> String
  , transform   -- :: Program Surface -> Program ANF
  ) where

import Data.Traversable as T

import Language.Typo.ASTs


compile :: Program Surface -> String
compile = show . transform

transform :: Program Surface -> Program ANF
transform = runGensym . T.mapM (runAnf . anormalize)
