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

-- GHC Modules
import GHC ( runGhc, getSessionDynFlags )
import GHC.Paths ( libdir )
import HsSyn
import Outputable ( Outputable(ppr), showSDocForUser, neverQualify )
import SrcLoc ( noLoc )
import RdrName ( RdrName )


compile :: TypoConfig -> Program Surface -> IO String
compile config = compile'
  where
    compile' | tc_aNormalize config = return . compileAnf
             | otherwise            = complete . compileHs . transform
    complete | tc_noPrelude config = id
             | otherwise           = fmap (prelude ++)

transform :: Program Surface -> Program ANF
transform = runGensym . T.mapM (runAnf . anormalize)

compileAnf :: Program Surface -> String
compileAnf = prettyRender . transform

transformHs :: Program ANF -> HsModule RdrName
transformHs p = modwrap (concatMap D.definition (definitions p) ++ [cexpr (expr p)])
  where
    modwrap ds = HsModule Nothing Nothing [] ds Nothing Nothing
    cexpr e = noLoc (ValD (VarBind (E.mkName "result") (E.anf e) False))

compileHs :: Program ANF -> IO String
compileHs = render . ppr . transformHs
  where
    render d = runGhc (Just libdir) $ do
        df <- getSessionDynFlags
        return (showSDocForUser df neverQualify d)
