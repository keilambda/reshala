module Reshala.SAT.Solver.Naive
  ( sat
  , sol
  ) where

import Control.Applicative
import Data.Functor.Foldable
import Data.Map.Strict qualified as Map
import Pre
import Reshala.SAT
import Reshala.SAT.Expr

pop :: Expr -> Maybe Var
pop = cata \case
  LitF _ -> Nothing
  VarF v -> Just v
  NotF a -> a
  a :&:$ b -> a <|> b
  a :|:$ b -> a <|> b

subst :: Var -> Bool -> Expr -> Expr
subst var val = cata \case
  VarF v | v == var -> Lit val
  e -> embed e

eval :: Expr -> Bool
eval = cata \case
  LitF b -> b
  VarF _ -> undefined
  NotF b -> not b
  a :&:$ b -> a && b
  a :|:$ b -> a || b

sol :: Expr -> List Solution
sol = loop mempty
 where
  loop env expr = case pop expr of
    Nothing -> [env | eval expr]
    Just var -> do
      let true = subst var True expr
      let false = subst var False expr
      loop (Map.insert var True env) true <> loop (Map.insert var False env) false

sat :: Expr -> Bool
sat = not . null . sol

newtype Naive = MkNaive Expr

instance Solver Naive where
  solutions (MkNaive s) = sol s
