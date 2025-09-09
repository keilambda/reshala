module Reshala.SAT.CNF
  ( Atom (..)
  , Clause
  , Formula
  , CNF (..)
  , fromExpr
  ) where

import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Pre
import Reshala.SAT.Expr

data Atom
  = Pos Var
  | Neg Var
  deriving stock (Eq, Ord, Show)

type Clause = NESet Atom
type Formula = Set Clause

data CNF
  = Bot
  | CNF Formula

fromExpr :: Expr -> CNF
fromExpr = \case
  Lit True -> CNF mempty
  Lit False -> Bot
  Var v -> CNF . Set.singleton . NESet.singleton $ Pos v
  Not e -> case e of
    Lit b -> fromExpr (Lit (not b))
    Var v -> CNF . Set.singleton . NESet.singleton $ Neg v
    Not a -> fromExpr a
    a :&: b -> fromExpr (Not a :|: Not b)
    a :|: b -> fromExpr (Not a :&: Not b)
  a :&: b -> fromExpr a `conj` fromExpr b
  a :|: b -> fromExpr a `disj` fromExpr b
 where
  conj = \cases
    Bot _ -> Bot
    _ Bot -> Bot
    (CNF a) (CNF b) -> CNF (a <> b)

  disj = \cases
    Bot b -> b
    a Bot -> a
    (CNF a) (CNF b)
      | Set.null a || Set.null b -> CNF mempty
      | otherwise -> CNF . Set.fromList . mapMaybe dropTauto $ [a' <> b' | a' <- toList a, b' <- toList b]

  dropTauto s
    | any (\v -> NESet.member (Pos v) s && NESet.member (Neg v) s) vars = Nothing
    | otherwise = Just s
   where
    vars = map (\case Pos v -> v; Neg v -> v) (toList s)
