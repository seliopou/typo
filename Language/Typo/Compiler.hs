module Language.Typo.Compiler
  ( compile     -- :: Program Surface -> String
  , compileAnf  -- :: Program Surface -> String
  , transform   -- :: Program Surface -> Program ANF
  ) where

import Control.Applicative ( (<$>) )

import Data.Traversable as T

import Language.Typo.ASTs
import Language.Typo.Prelude ( prelude )
import Language.Typo.PrettyPrint

import qualified Language.Typo.Compiler.Expression as E

-- GHC Modules
import GHC ( runGhc, getSessionDynFlags )
import GHC.Paths ( libdir )
import HsSyn
import Outputable ( Outputable(ppr), showSDocForUser, neverQualify )
import SrcLoc ( noLoc )
import RdrName ( RdrName )


compile :: Program Surface -> IO String
compile = complete . render . ppr . compileHs . transform
  where
    complete = ((prelude ++) <$>)
    render d = runGhc (Just libdir) $ do
        df <- getSessionDynFlags
        return (showSDocForUser df neverQualify d)

compileAnf :: Program Surface -> String
compileAnf = prettyRender . transform

transform :: Program Surface -> Program ANF
transform = runGensym . T.mapM (runAnf . anormalize)

compileHs :: Program ANF -> HsModule RdrName
compileHs p = modwrap (map cdecl (definitions p) ++ [cexpr (expr p)])
  where
    modwrap ds = HsModule Nothing Nothing [] ds Nothing Nothing
    cexpr e = noLoc (ValD (VarBind (E.mkName "result") (E.anf e) False))
    cdecl d = undefined
