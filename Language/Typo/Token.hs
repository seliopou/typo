{-# LANGUAGE FlexibleContexts #-}
module Language.Typo.Token
  ( typoDef     -- :: LanguageDef s
  , typo        -- :: GenTokenParser String u Identity
  , lexeme      -- :: Parsec String u a -> Parsec String u a
  , parens      -- :: Parsec String u a -> Parsec String u a
  , identifier  -- :: Parsec String u String
  , operator    -- :: Parsec String u String
  , natural     -- :: Parsec String u Integer
  , whiteSpace  -- :: Parsec String u ()
  ) where

import Control.Monad.Identity

import Text.Parsec ( oneOf )
import Text.Parsec.Prim
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.Token ( GenTokenParser, LanguageDef, makeTokenParser )
import qualified Text.Parsec.Token as P


typoDef :: LanguageDef s
typoDef = emptyDef {
  P.commentLine = ";",
  P.opStart = P.opLetter typoDef,
  P.opLetter = oneOf ":!$%&*+./<=>?@\\^|-~",
  P.reservedNames = [
      "define", "let", "if"             -- language keywords
    , "and", "or", "imp", "cond"        -- (prelude) boolean operators
    , "add", "sub", "mul", "div", "rem" -- (prelude) arithmetic operators
    , "eq", "lt"                        -- (prelude) comparison operators
    , "result", "res", "undefined"      -- program keywords
    ]
}

typo :: GenTokenParser String u Identity
typo = makeTokenParser typoDef

lexeme, parens :: Parsec String u a -> Parsec String u a
lexeme = P.lexeme typo
parens = P.parens typo

identifier, operator :: Parsec String u String
identifier = P.identifier typo
operator = P.operator typo

natural :: Parsec String u Integer
natural = P.natural typo

whiteSpace :: Parsec String u ()
whiteSpace = P.whiteSpace typo
