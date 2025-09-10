module Reshala.SAT
  ( Solver (..)
  ) where

import Data.Maybe
import Pre
import Reshala.SAT.Expr

class Solver s where
  solutions :: s -> List Solution

  solution :: s -> Maybe Solution
  solution = listToMaybe . solutions

  satisfiable :: s -> Bool
  satisfiable = not . null . solutions
  {-# MINIMAL solutions #-}
