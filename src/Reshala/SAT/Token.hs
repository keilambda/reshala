{-# LANGUAGE OverloadedStrings #-}

module Reshala.SAT.Token
  ( P
  , Tok (..)
  , toks
  , run
  ) where

import Data.Text qualified as Text
import Data.Void (Void)
import Pre hiding (Bool (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

type P = Parsec Void Text

data Tok
  = True
  | False
  | Ident Text
  | Not
  | And
  | Or
  | Xor
  | Imp
  | Iff
  | LPar
  | RPar
  deriving stock (Eq, Ord, Show)

sc :: P ()
sc = Lex.space space1 (Lex.skipLineComment "--") (Lex.skipBlockComment "/*" "*/")

toks :: P (List Tok)
toks = do
  sc
  ts <- many (tok <* sc)
  eof
  pure ts

tok :: P Tok
tok =
  choice
    [ Iff <$ string "<->"
    , Imp <$ string "->"
    , And <$ string "&&"
    , Or <$ string "||"
    , Xor <$ char '^'
    , Not <$ char '!'
    , LPar <$ char '('
    , RPar <$ char ')'
    , kwOrIdent <?> "keyword or identifier"
    ]

kwOrIdent :: P Tok
kwOrIdent = do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let txt = Text.pack (first : rest)
  pure case txt of
    "true" -> True
    "false" -> False
    _ -> Ident txt

run :: FilePath -> Text -> Either String (List Tok)
run filepath = over _Left errorBundlePretty . runParser (sc *> toks <* sc <* eof) filepath
