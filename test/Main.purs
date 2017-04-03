module Test.Main where

import Prelude (flip, ($), (==), (>=), (<=), (++), (+), show)
import Test.Spec (pending, describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.Spec.QuickCheck (quickCheckPure)
import Test.QuickCheck as QC

import Data.Set as Set
import Data.Foldable (foldl)
import Data.Int53 as Int53

import Concelo.IncrementalMerkelTree as Tree
import Concelo.IncrementalMerkelTree (add, remove)

main = run [consoleReporter] do
  let seed = QC.mkSeed 42
      count = 100
      check description predicate =
        it description $ quickCheckPure seed count predicate

  describe ("using seed " ++ show seed ++ " and count " ++ show count) do
    describe "incremental set hashing" do
      check "assigns equal hashes to equal sets" \set ->
        verify set (tree set)

      check "assigns unequal hashes to unequal sets" \set1 set2 ->
        if set1 == set2 then
          Success
        else
          Tree.hash (tree set1) /== Tree.hash (tree set2)

      check "adds a single element incrementally" \set element ->
        verify (Set.add element set) (add element (tree set))

      check "removes a single element incrementally" \set element ->
        verify (Set.delete element set) (remove element (tree set))

      check "adds multiple elements incrementally" \set elements ->
        verify (Set.union set elements) (addAll elements (tree set))

      check "removes multiple elements incrementally" \set elements ->
        verify (Set.difference set elements) (removeAll elements (tree set))

      check "updates incrementally without thrashing" \set element ->
        -- After we add and remove an element once, the depth of the
        -- tree should not change if we subsequently add and remove
        -- that element repeatedly.
        let addRemove = add element >>> remove element

            noThrash t _ =
              let t' = addRemove t in
              if Tree.depth t' == Tree.depth t then
                Right t'
              else
                Left "adding and removing an element causes unexpected refit"

        in assert
           $ foldM noThrash (Right $ addRemove $ tree $ Set.delete element set)
           [1..10]

tree elements = addAll elements Tree.empty

addAll elements tree = foldl (flip add) tree elements

removeAll elements tree = foldl (flip remove) tree elements

assert = case _ of
  Left message -> Failure message
  Right _ -> Success

equal a b
  | a == b = Right unit
  | otherwise = Left (show a ++ " is not equal to " ++ show b)

lessThanOrEqual a b
  | a <= b = Right unit
  | otherwise = Left (show a ++ " is greater than " ++ show b)

greaterThanOrEqual a b
  | a >= b = Right unit
  | otherwise = Left (show a ++ " is less than " ++ show b)

verify elements tree =
  let setSize = Set.size elements
      treeSize = Tree.size tree
      treeDepth = Tree.depth tree
      setTree = tree elements
      setTreeDepth = Tree.depth setTree
      powerOfTwo = Int53.pow $ Int53.fromInt 2

  in assert do
    treeSize `equal` setSize

    treeSize `lessThanOrEqual` (powerOfTwo (treeDepth + 1))

    treeSize `greaterThanOrEqual` (powerOfTwo treeDepth)

    Tree.hash (Tree.refit treeDepth setTree) `equal` Tree.hash tree

    Tree.hash setTree `equal` Tree.hash (Tree.refit setTreeDepth tree)
