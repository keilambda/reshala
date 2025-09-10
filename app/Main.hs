{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative
import Pre hiding (argument)
import Reshala.SAT.Expr
import Reshala.SAT.Parser qualified as Parser
import Reshala.SAT.Solver.Naive qualified as Naive
import Reshala.SAT.Solver.Table qualified as Table
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Solver
  = Naive
  | Table

data Cmd
  = SatCmd Solver Text
  | SolCmd Solver Text

main :: IO ()
main =
  execParser opts >>= \case
    SatCmd s txt -> do
      sats <- sat s <$> parseOrDie txt
      Text.putStrLn if sats then "SAT" else "UNSAT"
    SolCmd s txt -> do
      sols <- sol s <$> parseOrDie txt
      if null sols
        then Text.putStrLn "UNSAT"
        else for_ sols (Text.putStrLn . renderSolution)
 where
  opts = info (helper <*> cmd) (fullDesc <> progDesc "Reshala - SAT solver")

  sat = \case
    Naive -> Naive.sat
    Table -> Table.sat

  sol = \case
    Naive -> Naive.sol
    Table -> Table.sol

  renderSolution m
    | Map.null m = "{}"
    | otherwise = Text.unwords . map showPair . Map.toAscList $ m

  showPair (k, v) = k <> "=" <> (if v then "true" else "false")

cmd :: Parser Cmd
cmd =
  hsubparser . mconcat
    $ [ command "sat" (info (SatCmd <$> solver <*> expr) (progDesc "Check satisfiability"))
      , command "sol" (info (SolCmd <$> solver <*> expr) (progDesc "List satisfying assignments"))
      ]

solver :: Parser Solver
solver =
  asum
    [ flag' Naive (long "naive" <> help "Use naive backtracking solver")
    , flag Table Table (long "table" <> help "Use truth-table solver (default)")
    ]

expr :: Parser Text
expr = argument str (metavar "EXPR" <> help "Boolean expression. Use quotes in shells.")

parseOrDie :: Text -> IO Expr
parseOrDie t = case Parser.run "stdin" t of
  Left err -> do
    hPutStrLn stderr $ "Parse error:\n" <> err
    exitFailure
  Right e -> pure e
