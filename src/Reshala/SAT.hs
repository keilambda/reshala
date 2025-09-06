{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reshala.SAT
  ( Expr (..)
  , free
  , subst
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

free :: Expr -> Maybe Char
free = cata alg
 where
  alg = \case
    LitF _ -> Nothing
    VarF v -> Just v
    NotF a -> a
    a :&$ b -> a <|> b
    a :|$ b -> a <|> b

subst :: Char -> Bool -> Expr -> Expr
subst var val = cata alg
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

deMorgan :: Expr -> Maybe Expr
deMorgan = \case
  Not (a :& b) -> Just (Not a :| Not b)
  Not (a :| b) -> Just (Not a :& Not b)
  _ -> Nothing

simp :: Expr -> Expr
simp = rewrite rws
 where
  rws e = asum (map ($ e) rules)
  rules =
    [ step
    , deMorgan
    ]

sat :: Expr -> Bool
sat expr = case free expr of
  Nothing -> case expr of
    Lit b -> b
    _ -> error "sat: not a Lit"
  Just v ->
    let true = simp (subst v True expr)
        false = simp (subst v False expr)
     in sat true || sat false
