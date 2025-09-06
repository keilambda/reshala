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
import Data.Maybe (fromMaybe)
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

type Rewrite = Expr -> Maybe Expr

step :: Rewrite
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

deMorgan :: Rewrite
deMorgan = \case
  Not (a :& b) -> Just (Not a :| Not b)
  Not (a :| b) -> Just (Not a :& Not b)
  _ -> Nothing

distribute :: Rewrite
distribute = \case
  a :| (b :& c) -> Just (a :| b :& a :| c)
  (a :& b) :| c -> Just (a :| c :& b :| c)
  _ -> Nothing

simp :: Expr -> Expr
simp = rewrite step

apply :: Rewrite -> (Expr -> Expr)
apply rule expr = fromMaybe expr (rule expr)

toNNF :: Expr -> Expr
toNNF = transform (apply deMorgan)

distributeCNF :: Expr -> Expr
distributeCNF = transform (apply distribute)

normalize :: Expr -> Expr
normalize = simp . distributeCNF . toNNF . simp

sat :: Expr -> Bool
sat expr = case free expr of
  Nothing -> case expr of
    Lit b -> b
    _ -> error "sat: not a Lit"
  Just v ->
    let true = normalize (subst v True expr)
        false = normalize (subst v False expr)
     in sat true || sat false
