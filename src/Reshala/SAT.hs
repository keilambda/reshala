{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reshala.SAT
  ( Expr (..)
  , free
  , vars
  , subst
  , simp
  , sat
  , satDPLL
  , polarity
  , elim
  , unitPropagation
  ) where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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

vars :: Expr -> Set Char
vars = foldMap alg . universe
 where
  alg (Var v) = Set.singleton v
  alg _ = mempty

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

simp :: Expr -> Expr
simp = rewrite step

distribute :: Expr -> Expr
distribute = cata alg
 where
  alg = \case
    a :|$ (b :& c) -> (a :| b) :& (a :| c)
    (a :& b) :|$ c -> (a :| c) :& (b :| c)
    expr -> embed expr

toNNF :: Expr -> Expr
toNNF = cata alg
 where
  alg = \case
    NotF (Not e) -> e
    NotF (a :& b) -> Not a :| Not b
    NotF (a :| b) -> Not a :& Not b
    NotF (Lit b) -> Lit (not b)
    expr -> embed expr

cnf :: Expr -> Expr
cnf expr = if updated == expr then expr else cnf updated
 where
  updated = distribute (toNNF expr)

data Polarity = Pos | Neg | Mix
  deriving stock (Eq)

polarity :: Expr -> Char -> Maybe Polarity
polarity (Var v) v'
  | v == v' = Just Pos
  | otherwise = Nothing
polarity (Not (Var v)) v'
  | v == v' = Just Neg
  | otherwise = Nothing
polarity expr v = case expr of
  a :& b -> comb [a, b]
  a :| b -> comb [a, b]
  Not a -> error $ "polarity: not in CNF: negation of a non-literal: " ++ show a
  Lit _ -> Nothing
 where
  comb es = case mapMaybe (`polarity` v) es of
    [] -> Nothing
    ps -> if all (== Pos) ps then Just Pos else if all (== Neg) ps then Just Neg else Just Mix

elim :: Expr -> Expr
elim expr =
  let ls = toList (vars expr)
      ps = map (polarity expr) ls
      as = catMaybes (zipWith extract ls ps)
      rs = map (uncurry subst) as
   in foldl' (.) id rs expr
 where
  extract v = \case
    Just Pos -> Just (v, True)
    Just Neg -> Just (v, False)
    _ -> Nothing

clauses :: Expr -> List Expr
clauses (a :& b) = clauses a ++ clauses b
clauses expr = [expr]

unitClauses :: Expr -> List (Char, Bool)
unitClauses = mapMaybe f . clauses
 where
  f = \case
    (Var v) -> Just (v, True)
    (Not (Var v)) -> Just (v, False)
    _ -> Nothing

unitPropagation :: Expr -> Expr
unitPropagation expr = foldl' (.) id (map (uncurry subst) (unitClauses expr)) expr

unLit :: Expr -> Bool
unLit = \case Lit b -> b; _ -> error "unLit: not a Lit"

sat :: Expr -> Bool
sat expr = case free expr of
  Nothing -> unLit expr
  Just v ->
    let true = simp (subst v True expr)
        false = simp (subst v False expr)
     in sat true || sat false

satDPLL :: Expr -> Bool
satDPLL expr = case free expr' of
  Nothing -> unLit (simp expr')
  Just v ->
    let true = simp (subst v True expr)
        false = simp (subst v False expr)
     in satDPLL true || satDPLL false
 where
  expr' = elim . cnf . unitPropagation $ expr
