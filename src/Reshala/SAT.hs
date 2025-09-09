module Reshala.SAT
  ( Solver (..)
  ) where

import Pre

class Solver s where
  solve :: s -> Bool
