{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative
import Pre hiding (argument)
import Reshala.SAT
import Reshala.SAT.Expr
import Reshala.SAT.Parser qualified as Parser
import Reshala.SAT.Solver.Naive (Naive (..))
import Reshala.SAT.Solver.Table (Table (..))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data SolverChoice
  = UseNaive
  | UseTable

data Cmd
  = SatCmd SolverChoice Text
  | SolCmd SolverChoice Text

main :: IO ()
main =
  execParser opts >>= \case
    SatCmd s txt -> do
      e <- parseOrDie txt
      let ok = withSolver s \mk -> satisfiable (mk e)
      Text.putStrLn (if ok then "SAT" else "UNSAT")
    SolCmd s txt -> do
      e <- parseOrDie txt
      let sols = withSolver s \mk -> solutions (mk e)
      if null sols
        then Text.putStrLn "UNSAT"
        else for_ sols (Text.putStrLn . renderSolution)
 where
  opts = info (helper <*> cmd) (fullDesc <> progDesc "Reshala - SAT solver")

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

solver :: Parser SolverChoice
solver =
  asum
    [ flag' UseNaive (long "naive" <> help "Use naive backtracking solver")
    , flag UseTable UseTable (long "table" <> help "Use truth-table solver (default)")
    ]

expr :: Parser Text
expr = argument str (metavar "EXPR" <> help "Boolean expression. Use quotes in shells.")

parseOrDie :: Text -> IO Expr
parseOrDie t = case Parser.run "stdin" t of
  Left err -> do
    hPutStrLn stderr $ "Parse error:\n" <> err
    exitFailure
  Right e -> pure e

withSolver :: SolverChoice -> (forall s. (Solver s) => (Expr -> s) -> r) -> r
withSolver = \case
  UseNaive -> \k -> k MkNaive
  UseTable -> \k -> k MkTable
