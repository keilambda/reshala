{-# LANGUAGE OverloadedStrings #-}

module Reshala.SAT.Parser
  ( run
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import Pre
import Reshala.SAT.Expr
import Reshala.SAT.Token (Tok)
import Reshala.SAT.Token qualified as Tok
import Text.Megaparsec hiding (parse)

type P = Parsec Void (List Tok)

term :: P Expr
term =
  choice
    [ Lit True <$ single Tok.True
    , Lit False <$ single Tok.False
    , var
    , between (single Tok.LPar) (single Tok.RPar) expr
    ]

var :: P Expr
var =
  satisfy isIdent >>= \case
    Tok.Ident txt -> pure (Var txt)
    _ -> fail "unreachable"
 where
  isIdent = \case Tok.Ident _ -> True; _ -> False

table :: List (List (Operator P Expr))
table =
  [ [Prefix (Not <$ single Tok.Not)]
  , [InfixL ((:&:) <$ single Tok.And)]
  , [InfixL ((:|:) <$ single Tok.Or)]
  , [InfixL (xor <$ single Tok.Xor)]
  , [InfixR (imp <$ single Tok.Imp)]
  , [InfixR (iff <$ single Tok.Iff)]
  ]
 where
  xor a b = (a :|: b) :&: Not (a :&: b)
  imp a b = Not a :|: b
  iff a b = (a :&: b) :|: (Not a :&: Not b)

expr :: P Expr
expr = makeExprParser term table

run :: FilePath -> Text -> Either String Expr
run filepath input = do
  ts <- Tok.run filepath input
  over _Left show . runParser (expr <* eof) filepath $ ts
