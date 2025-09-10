module Main (main) where

import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Reshala.SAT.Expr
import Reshala.SAT.Solver.Naive qualified as Naive
import Reshala.SAT.Solver.Table qualified as Table
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "reshala"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "unit"
    [ testCase "Lit True is satisfiable" do
        let e = Lit True
        Naive.sat e @?= True
        Table.sat e @?= True
        Naive.sol e @?= [mempty]
        Table.sol e @?= [mempty]
    , testCase "Lit False is unsatisfiable" do
        let e = Lit False
        Naive.sat e @?= False
        Table.sat e @?= False
        Naive.sol e @?= []
        Table.sol e @?= []
    , testCase "a OR not a is a tautology" do
        let a = Var 'a'
        let e = a :|: Not a
        Naive.sat e @?= True
        Table.sat e @?= True
        let solsN = Set.fromList (Naive.sol e)
        let solsT = Set.fromList (Table.sol e)
        let all2 = Set.fromList [Map.fromList [('a', False)], Map.fromList [('a', True)]]
        solsN @?= all2
        solsT @?= all2
    , testCase "a AND not a is a contradiction" do
        let a = Var 'a'
        let e = a :&: Not a
        Naive.sat e @?= False
        Table.sat e @?= False
        Naive.sol e @?= []
        Table.sol e @?= []
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "Naive.sat == Table.sat" prop_equivSat
    , testProperty "Naive.sol == Table.sol" prop_equivSolutions
    , testProperty "Every Naive solution satisfies expr" prop_naiveSolutionsSatisfy
    , testProperty "Every Table solution satisfies expr" prop_tableSolutionsSatisfy
    ]

genVar :: Gen Var
genVar = Gen.element ['a' .. 'd']

genExpr :: Gen Expr
genExpr =
  Gen.recursive
    Gen.choice
    [ Lit <$> Gen.bool
    , Var <$> genVar
    ]
    [ Not <$> genExpr
    , liftA2 (:&:) genExpr genExpr
    , liftA2 (:|:) genExpr genExpr
    ]

evalWith :: Solution -> Expr -> Bool
evalWith env = \case
  Lit b -> b
  Var v -> Map.findWithDefault False v env
  Not a -> not (evalWith env a)
  a :&: b -> evalWith env a && evalWith env b
  a :|: b -> evalWith env a || evalWith env b

prop_equivSat :: Property
prop_equivSat = property do
  e <- forAll genExpr
  Naive.sat e === Table.sat e

prop_equivSolutions :: Property
prop_equivSolutions = property do
  e <- forAll genExpr
  Set.fromList (Naive.sol e) === Set.fromList (Table.sol e)

prop_naiveSolutionsSatisfy :: Property
prop_naiveSolutionsSatisfy = property do
  e <- forAll genExpr
  for_ (Naive.sol e) \env -> evalWith env e === True

prop_tableSolutionsSatisfy :: Property
prop_tableSolutionsSatisfy = property do
  e <- forAll genExpr
  for_ (Table.sol e) \env -> evalWith env e === True
