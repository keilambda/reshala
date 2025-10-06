module Reshala.Ya.AST where

import Ya
import Ya.ASCII
import Data.Set (Set, insert)

import Reshala.Ya.Instances ()

type Variable = Nonempty List `T'I` Letter

type Dyadic = Unit `S` Unit

pattern AND x = This x :: Dyadic
pattern OR x = That x :: Dyadic

type Expression = Final Dyadic `P'T'I'TT'I` Twice `S'T'I'TT'I` Alone

pattern Expression x xx = LT (LRT (Final x) (Twice xx)) :: Expression e
pattern Conjunct x = LT (LRT (Final (AND Unit)) (Twice x)) :: Expression e
pattern Disjunct x = LT (LRT (Final (OR Unit)) (Twice x)) :: Expression e
pattern Negation x = RT (Alone x) :: Expression e

type Value = Variable `S` Boolean

pattern Variable x = This x :: Value
pattern Literal x = That x :: Value

first_free_variable :: Instruction Expression Value `AR___` Stops `T'I` Variable `T'I` Instruction Expression Boolean
first_free_variable x = x `yokl` Forth `ha` Check `ha__` Break @Variable `la` Going @Boolean

-- TODO: Ditch last {`ho_'ho` These Unit} by using core data structures
gather_all_variables :: Instruction Expression Value `AR___` State `T'I` Set Variable `T'I` Instruction Expression Unit
gather_all_variables x = x `yokl` Forth `ha` Apply `ha` State `ha` Event `ha__` insert @Variable `la` Literal `hu` Same `ho_'ho` These Unit

-- IDEA: Try to make it more explicit with `Match` functor
substitute_single_variable :: Variable `P` Boolean `AR__` Instruction Expression Value `AR_` Instruction Expression Value
substitute_single_variable (These name value) x = x `yo_` this `ho` Variable `la` Variable `hu` Literal value `ha_` (`lu'q` name) `la` Literal
