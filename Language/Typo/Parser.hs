module Language.Typo.Parser
  ( Language.Typo.Parser.parse  -- :: FilePath -> String -> Either ParseError (Program Surface)
  , program         -- :: Parsec String u Program
  , definition      -- :: Parsec String u Definition
  , surface         -- :: Parsec String u Surface
  , value           -- :: Parsec String u Value
  , operator        -- :: Parsec String u Op
  , operatorTable   -- :: [(String, Op)]
  ) where

import Control.Applicative ( (<$>), (<*>), (*>) )

import qualified Language.Typo.Token as T
import Language.Typo.ASTs

import Text.Parsec


parse :: FilePath -> String -> Either ParseError (Program Surface)
parse = Text.Parsec.parse program

program :: Parsec String u (Program Surface)
program = T.whiteSpace *> (Program <$> (many definition) <*> surface)

-- N.B. `definition` cannot use the parens combinator because the `program`
-- parser needs to be able to backtrack past the left paren. If `definition`
-- did use the parens combinator, a `try` would have to wrap it. Instead,
-- the implementation uses a left paren parser and a string parser for the
-- "def" keyword, and wraps that sequence with `try`.
--
definition :: Parsec String u (Definition Surface)
definition = between start end
  ((T.parens $ Definition <$> T.identifier <*> (many T.identifier)) <*> surface)
  where
    start = try (lparen *> T.lexeme (string "define"))
    end = rparen
    lparen = T.lexeme (char '(')
    rparen = T.lexeme (char ')')

surface :: Parsec String u Surface
surface = (T.parens nonval) <|> val
  where
    nonval = bop <|> (try $ _let <|> cond) <|> app
    _let = do
        T.lexeme (try $ string "let")
        T.parens (Let <$> T.identifier <*> surface) <*> surface
    cond = do
        T.lexeme (try $ string "if" )
        Cond <$> surface <*> surface <*> surface
    bop = Bop <$> operator <*> surface <*> surface
    app = App <$> T.identifier <*> (many surface)
    val = Val <$> value

value :: Parsec String u Value
value = num <|> bool <|> ident
  where
    num = Number <$> fromIntegral <$> T.natural
    bool = T.lexeme $ Boolean <$> do
      char '#'
      (char 't' *> return True) <|> (char 'f' *> return False)
    ident = Id <$> T.identifier

operator :: Parsec String u Op
operator = do
  op <- T.operator
  case lookup op operatorTable of
    Just op' -> return op'
    Nothing  -> fail $ "unrecognized operator: " ++ op

operatorTable :: [(String, Op)]
operatorTable = [
    ("+", Add)
  , ("-", Sub)
  , ("*", Mul)
  , ("/", Div)
  , ("%", Rem)
  , ("&&", And)
  , ("||", Or)
  , ("->", Imp)
  , ("==", Eq)
  , ("<", Lt)
  ]
