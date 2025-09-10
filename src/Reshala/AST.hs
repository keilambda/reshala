module Reshala.AST where

import Ya
import Ya.ASCII

type Variable = Nonempty List `T'I` Letter

type Operator = Unit `S` Unit

pattern AND, OR :: Operator
pattern AND = This Unit
pattern OR = That Unit

type Operation = Final Operator `P'T'I'TT'I` Twice

type Expression = Operation `S'T'I'TT'I` Alone `S'T'I'TT'I` Final Variable `S'T'I'TT'I` Final Boolean

pattern Literal x = RT (Final x) :: Expression e
pattern Variable x = LT (RT (Final x)) :: Expression e
pattern Negation x = LT (LT (RT (Alone x))) :: Expression e
pattern Conjunct x = LT (LT (LT (LRT (Final AND) (Twice x)))) :: Expression e
pattern Disjunct x = LT (LT (LT (LRT (Final OR) (Twice x)))) :: Expression e

pattern Compound op x = LT (LT (LT (LRT (Final op) (Twice x)))) :: Expression e

first_free_variable :: Recursive Expression `AR___` Maybe Variable
first_free_variable = cata `hv` \case
 Compound _ x -> x `ho` First `ys'yo` (is `la` is)
 Negation x -> x
 Variable x -> Some x
 Literal _ -> None Unit
