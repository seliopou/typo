module Main ( main ) where

import Control.Monad ( when )

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Language.Typo.Config
import Language.Typo.Compiler ( compile )
import Language.Typo.Parser ( parse )


main :: IO ()
main = do
    (fs, _) <- arguments

    when (Help `elem` fs) $ do
      putStrLn help
      exitSuccess

    let config = fromFlags [f | TCFlag f <- fs]
    text <- hGetContents stdin
    case parse "<stdin>" text of
      Left  error   -> print error
      Right program -> do
        result <- compile config program
        putStr result

data Flag = TCFlag TypoFlag | Help
  deriving ( Eq, Ord )

arguments :: IO ([Flag], [String])
arguments = do
  (fs, as, errs) <- (getOpt RequireOrder options) `fmap` getArgs 
  case errs of
    []-> return (fs, as)
    _ -> do
      putStr (concat errs ++ help)
      exitFailure

options :: [OptDescr Flag]
options =
  [ Option [] ["anf"]        (NoArg (TCFlag ANormalize)) "print A-normalized program"
  [ Option [] ["racket"]     (NoArg (TCFlag Racket))     "compile to racket"
  , Option [] ["no-prelude"] (NoArg (TCFlag NoPrelude))  "do not add prelude to compiled code"
  ]

help :: String
help = usageInfo header options
  where header = "USAGE: typoc OPTS"
