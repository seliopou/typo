module Main ( main ) where

import Control.Monad ( when )

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Language.Typo.Compiler ( compile, compileAnf )
import Language.Typo.Parser ( parse )


main :: IO ()
main = do
    (fs, _) <- arguments

    when (Help `elem` fs) $ do
      putStrLn help
      exitSuccess

    let compiler | ANF `elem` fs = return . compileAnf
                 | otherwise     = compile

    text <- hGetContents stdin
    case parse "<stdin>" text of
      Left  error   -> print error
      Right program -> do
        result <- compiler program
        putStr result

data Flag = ANF | Help
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
options = [ Option [] ["anf"] (NoArg ANF) "print A-normalized program" ]

help :: String
help = usageInfo header options
  where header = "USAGE: typoc OPTS"
