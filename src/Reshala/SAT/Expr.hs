{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reshala.SAT.Expr
  ( Var
  , Expr (..)
  , ExprF (..)
  , Solution
  ) where

import Data.Data (Data)
import Data.Functor.Foldable.TH
import Pre

type Var = Text

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
type ExprF :: Type -> Type

type Solution = Map Var Bool
