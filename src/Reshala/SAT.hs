{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reshala.SAT
  ( Var
  , Expr (..)
  , sat
  ) where

import Control.Applicative
import Data.Data (Data)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Pre

type Var = Char

data Expr
  = Lit Bool
  | Var Var
  | Not Expr
  | Expr :&: Expr
  | Expr :|: Expr
  deriving stock (Data, Eq, Generic, Show)
  deriving anyclass (Plated)

infixl 7 :&:
infixl 6 :|:

makeBaseFunctor ''Expr

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
