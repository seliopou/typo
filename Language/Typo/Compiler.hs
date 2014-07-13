module Language.Typo.Compiler
  ( compile     -- :: TypoConfig -> Program Surface -> IO String
  , compileHs   -- :: Program Surface -> IO String
  , compileAnf  -- :: Program Surface -> String
  , transform   -- :: Program Surface -> Program ANF
  , transformHs -- :: Program ANF -> HsModule RdrName
  , module Language.Typo.Config
  ) where

import Data.Traversable as T

import Language.Typo.ASTs
import Language.Typo.Config
import Language.Typo.Prelude ( prelude )
import Language.Typo.PrettyPrint

import qualified Language.Typo.Compiler.Expression as E
import qualified Language.Typo.Compiler.Definition as D

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr


compile :: TypoConfig -> Program Surface -> String
compile config = compile'
  where
    compile' | tc_aNormalize config = compileAnf
             | otherwise            = complete . compileHs . transform
    complete | tc_noPrelude config = id
             | otherwise           = (prelude ++)

transform :: Program Surface -> Program ANF
transform = runGensym . T.mapM (runAnf . anormalize)

compileAnf :: Program Surface -> String
compileAnf = prettyRender . transform

transformHs :: Program ANF -> [Dec]
transformHs p = concatMap D.definition (definitions p) ++ [cexpr (expr p)]
  where
    cexpr e = ValD (VarP (mkName "result")) (NormalB (E.anf e)) []

compileHs :: Program ANF -> String
compileHs = pprint . transformHs
