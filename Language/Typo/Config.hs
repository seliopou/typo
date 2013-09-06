module Language.Typo.Config
  ( TypoFlag(..)
  , TypoConfig(..)
  , defaultConfig   -- :: TypoConfig
  , fromFlagsWith   -- :: TypoFlags -> [TypoFlags] -> TypoConfig
  , fromFlags       -- :: [TypoFlags] -> TypoConfig
  ) where


data TypoFlag
  = ANormalize
  | Racket
  | NoPrelude
  deriving ( Eq, Ord )

data TypoConfig = TypoConfig {
  tc_aNormalize :: Bool,
  tc_Racket :: Bool,
  tc_noPrelude :: Bool
} deriving ( Eq, Ord )

defaultConfig :: TypoConfig
defaultConfig = TypoConfig {
  tc_aNormalize = False,
  tc_Racket = False,
  tc_noPrelude = False
}

fromFlags :: [TypoFlag] -> TypoConfig
fromFlags = fromFlagsWith defaultConfig

fromFlagsWith :: TypoConfig -> [TypoFlag] -> TypoConfig
fromFlagsWith = foldl flag
  where
    flag c f =
      case f of
        ANormalize -> c { tc_aNormalize = True }
        NoPrelude -> c { tc_noPrelude = True }
        Racket -> c { tc_Racket = True }
