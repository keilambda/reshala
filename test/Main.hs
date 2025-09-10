module Main (main) where

import Data.Foldable
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hedgehog hiding (Var)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pre
import Reshala.SAT.CNF
import Reshala.SAT.CNF qualified as CNF
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
    [ testProperty "Naive.sat == Table.sat" (withTests 500 prop_equivSat)
    , testProperty "Naive.sol == Table.sol" (withTests 400 prop_equivSolutions)
    , testProperty "Naive solutions satisfy expr" (withTests 300 prop_naiveSolutionsSatisfy)
    , testProperty "Table solutions satisfy expr" (withTests 300 prop_tableSolutionsSatisfy)
    , testProperty "Solutions only assign used vars" (withTests 300 prop_solutionKeysSubset)
    , testProperty "No duplicate solutions" (withTests 300 prop_noDuplicates)
    , testProperty "Expr equivalent to CNF under any assignment" (withTests 400 prop_exprCnfEquiv)
    ]

genVar :: Gen Var
genVar = Gen.element ['a' .. 'z']

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
  Var v -> env ! v
  Not a -> not (evalWith env a)
  a :&: b -> evalWith env a && evalWith env b
  a :|: b -> evalWith env a || evalWith env b

varsOf :: Expr -> Set Var
varsOf = \case
  Lit _ -> mempty
  Var v -> Set.singleton v
  Not a -> varsOf a
  a :&: b -> varsOf a <> varsOf b
  a :|: b -> varsOf a <> varsOf b

genEnvFor :: Expr -> Gen Solution
genEnvFor expr = do
  let vs = toList (varsOf expr)
  bs <- Gen.list (Range.singleton (length vs)) Gen.bool
  pure (Map.fromList (zip vs bs))

evalCNF :: Solution -> CNF -> Bool
evalCNF env = \case
  Bot -> False
  CNF f
    | Set.null f -> True
    | otherwise -> all (any litTrue) f
 where
  litTrue = \case
    Pos v -> env ! v
    Neg v -> not (env ! v)

prop_equivSat :: Property
prop_equivSat = property do
  expr <- forAll genExpr
  Naive.sat expr === Table.sat expr

prop_equivSolutions :: Property
prop_equivSolutions = property do
  expr <- forAll genExpr
  Set.fromList (Naive.sol expr) === Set.fromList (Table.sol expr)

prop_naiveSolutionsSatisfy :: Property
prop_naiveSolutionsSatisfy = property do
  expr <- forAll genExpr
  for_ (Naive.sol expr) \env -> evalWith env expr === True

prop_tableSolutionsSatisfy :: Property
prop_tableSolutionsSatisfy = property do
  expr <- forAll genExpr
  for_ (Table.sol expr) \env -> evalWith env expr === True

prop_solutionKeysSubset :: Property
prop_solutionKeysSubset = property do
  expr <- forAll genExpr
  let vars = varsOf expr
  for_ (Naive.sol expr) \env -> Set.fromList (Map.keys env) `Set.isSubsetOf` vars === True
  for_ (Table.sol expr) \env -> Set.fromList (Map.keys env) `Set.isSubsetOf` vars === True

prop_noDuplicates :: Property
prop_noDuplicates = property do
  expr <- forAll genExpr
  let n = Naive.sol expr
  let t = Table.sol expr
  length n === Set.size (Set.fromList n)
  length t === Set.size (Set.fromList t)

prop_exprCnfEquiv :: Property
prop_exprCnfEquiv = property do
  expr <- forAll genExpr
  env <- forAll (genEnvFor expr)
  evalWith env expr === evalCNF env (CNF.fromExpr expr)
