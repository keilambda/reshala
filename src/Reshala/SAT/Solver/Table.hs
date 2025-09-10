module Reshala.SAT.Solver.Table
  ( sat
  , sol
  , Table (..)
  ) where

import Control.Monad (replicateM)
import Data.Functor.Foldable
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Pre
import Reshala.SAT
import Reshala.SAT.Expr

eval :: Solution -> Expr -> Bool
eval env = cata \case
  LitF b -> b
  VarF v -> env ! v
  NotF b -> not b
  a :&:$ b -> a && b
  a :|:$ b -> a || b

vars :: Expr -> Set Var
vars = foldMap alg . universe
 where
  alg = \case
    Var v -> Set.singleton v
    _ -> mempty

assignments :: List Var -> List Solution
assignments vs =
  [ Map.fromList (zip vs bs)
  | bs <- replicateM (length vs) [False, True]
  ]

sol :: Expr -> List Solution
sol e = filter (`eval` e) . assignments . toList . vars $ e

sat :: Expr -> Bool
sat = not . null . sol

newtype Table = MkTable Expr

instance Solver Table where
  solutions (MkTable e) = sol e
