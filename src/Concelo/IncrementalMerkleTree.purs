module Concelo.IncrementalMerkleTree
       ( Tree()
       , Depth()
       , empty
       , maybeDepth
       , depth
       , size
       , hash
       , add
       , remove
       , refit ) where

import Prelude (($), (<$>), (==), (+), (-), (>), (<), (<=), (>>>), (&&), map,
                otherwise, bind)
import Data.Int53 (Int53())
import Data.Int53 as Int53
import Data.Set (Set())
import Data.Set as Set
import Data.Array as Array
import Data.Lazy (Lazy())
import Data.Lazy as Lazy
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (foldl)
import Control.Monad (pure)
import Control.Monad.ST as ST

import Concelo.Interval as Interval
import Concelo.Crypto (Hash())
import Concelo.Crypto as Crypto

newtype Tree
  = Tree { root :: Node
         , depth :: Depth
         , size :: Int53 }

newtype Depth = Depth Int

data Node
  = Leaf (Set Hash)
  | Node { hash :: Lazy Hash
         , left :: Node
         , right :: Node }

data Operation
  = Add
  | Remove

empty = Tree { root: emptyLeaf
             , depth: Depth 0
             , size: Int53.fromInt 0 }

emptyLeaf = Leaf Set.empty

isEmptyLeaf = case _ of
  Leaf hashes -> Set.isEmpty hashes
  _ -> false

-- This must be no more than 53, since the algorithm below assumes we
-- can represent all non-negative integers from 0 to 2^maxDepth, and
-- that's only true for JS numbers (i.e. IEEE doubles) up to 2^53
maxDepth = 48

keyInterval = Interval.make
              (Int53.fromInt 0)
              (Int53.fromInt 2 `Int53.pow` Int53.fromInt maxDepth)

maybeDepth n = if n <= maxDepth then Just $ Depth n else Nothing

depth (Tree t) = t.depth

size (Tree t) = t.size

hash (Tree t) = hashNode t.root

hashNode = case _ of
      Leaf hashes -> Crypto.hash $ Array.fromFoldable hashes
      Node node -> Lazy.force node.hash

lazyHash nodes = Lazy.defer $ \_ -> Crypto.hash $ map hashNode nodes

add document tree = autoRefit $ applyDocument Add document tree

remove document tree = autoRefit $ applyDocument Remove document tree

autoRefit tree@(Tree t) = try t.depth
  where
    try depth
      | t.size > Int53.pow (Int53.fromInt 2) (depth + 1) =
        try (depth + 1) tree

      | depth > 0 && t.size < Int53.pow (Int53.fromInt 2) depth =
        try (depth - 1) tree

      | otherwise = refit depth tree

refit depth tree@(Tree t)
  | depth == t.depth = tree
  | otherwise = foldl add base tree
  where
    add tree hash = applyHash Add hash tree

    base = Tree { depth: depth
                , root: emptyLeaf
                , size: 0 }

applyDocument op = Array.singleton >>> Crypto.hash >>> applyHash op

applyHash op hash tree@(Tree t) = ST.pureST do
  sizeRef <- ST.newSTRef t.size

  root <- applyHashST op hash t.depth t.root sizeRef

  size <- ST.readSTRef sizeRef

  pure $ Tree { depth: t.depth
              , root: root
              , size: size }

applyHashST op hash depth root sizeRef = visit depth keyInterval root
  where
    key = Crypto.first48 hash

    visit depth interval node =
      let descend left right =
            if key <= Interval.midpoint interval then do
              left' <- visit (depth - 1) (Interval.leftHalf interval) left

              pure $ Node { hash: lazyHash [ left', right ]
                          , left: left'
                          , right: right }
            else do
              right' <- visit (depth - 1) (Interval.rightHalf interval) right

              pure $ Node { hash: lazyHash [ left, right' ]
                          , left: left
                          , right: right' }

          prune node@(Node n)
            | isEmptyLeaf n.left && isEmptyLeaf n.right = emptyLeaf
            | otherwise = node

      in case node of
        Leaf hashes -> case op of
          Add -> if depth == 0 then
                   if Set.member hash hashes then
                     pure node
                   else do
                     ST.modifySTRef sizeRef ((+) 1)

                     pure $ Leaf $ Set.insert hash hashes
                 else
                   descend emptyLeaf emptyLeaf

          Remove -> if Set.member hash hashes then
                      do
                        ST.modifySTRef sizeRef ((-) 1)

                        pure $ Leaf $ Set.delete hash hashes
                    else
                      pure node

        Node node -> prune <$> descend node.left node.right
