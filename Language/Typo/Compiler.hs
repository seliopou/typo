module Language.Typo.Compiler
  ( compile     -- :: Program Surface -> String
  , transform   -- :: Program Surface -> Program ANF
  ) where

import Data.Traversable as T

import Language.Typo.ASTs
import Language.Typo.PrettyPrint


compile :: Program Surface -> String
compile = prettyRender . transform

transform :: Program Surface -> Program ANF
transform = runGensym . T.mapM (runAnf . anormalize)
