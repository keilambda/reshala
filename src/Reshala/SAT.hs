module Reshala.SAT
  ( Var
  , Expr (..)
  , sat
  ) where

import Control.Applicative
import Data.Functor.Foldable
import Pre
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

sat :: Expr -> Bool
sat expr = case pop expr of
  Nothing -> eval expr
  Just var -> do
    let true = subst var True expr
    let false = subst var False expr
    sat true || sat false
