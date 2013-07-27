module Main ( main ) where

import System.IO

import Language.Typo.Compiler ( compile )
import Language.Typo.Parser ( parse )


main :: IO ()
main = do
    program <- hGetContents stdin
    case parse "<stdin>" program of
      Left  error  -> print error
      Right result -> putStrLn (compile result)
