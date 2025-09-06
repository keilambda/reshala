{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reshala.SAT
  ( Expr (..)
  , Clause
  , Formula
  , free
  , guess
  , simp
  , sat
  ) where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Pre

data Expr
  = Lit Bool
  | Var Char
  | Not Expr
  | Expr :& Expr
  | Expr :| Expr
  deriving stock (Data, Eq, Generic, Show)
  deriving anyclass (Plated)

infixl 7 :&
infixl 6 :|

makeBaseFunctor ''Expr

type Clause = List Expr
type Formula = List Clause

free :: Expr -> Maybe Char
free = cata alg
 where
  alg = \case
    LitF _ -> Nothing
    VarF v -> Just v
    NotF a -> a
    a :&$ b -> a <|> b
    a :|$ b -> a <|> b

guess :: Char -> Bool -> Expr -> Expr
guess var val = cata alg
 where
  alg (VarF v) | v == var = Lit val
  alg a = embed a

step :: Expr -> Maybe Expr
step = \case
  (Not (Lit b)) -> Just (Lit (not b))
  (Not (Not a)) -> Just a
  (Lit True :& a) -> Just a
  (a :& Lit True) -> Just a
  (Lit False :& _) -> Just (Lit False)
  (_ :& Lit False) -> Just (Lit False)
  (Lit False :| a) -> Just a
  (a :| Lit False) -> Just a
  (Lit True :| _) -> Just (Lit True)
  (_ :| Lit True) -> Just (Lit True)
  _ -> Nothing

simp :: Expr -> Expr
simp = rewrite step

sat :: Expr -> Bool
sat expr = case free expr of
  Nothing -> unLit expr
  Just v ->
    let true = simp (guess v True expr)
        false = simp (guess v False expr)
     in sat true || sat false
  where
    unLit = \case Lit b -> b; _ -> error "unLit: not a Lit"
