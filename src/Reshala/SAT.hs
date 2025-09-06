module Reshala.SAT () where

import Pre

data Expr
  = Lit Bool
  | Var Char
  | Not Expr
  | Expr :& Expr
  | Expr :| Expr

infixl 7 :&
infixl 6 :|

type Clause = List Expr
type Formula = List Clause
