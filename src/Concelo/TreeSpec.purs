module Concelo.TreeSpec
  ( make
  , key
  , toSpec
  , toTree
  , TreeSpec() ) where

import Concelo.Tree (Tree())
import qualified Concelo.Tree as T
import Prelude (Ord, compare, Show, show, ($), (++), (<<<), map)
import Data.Set (Set())
import qualified Data.Set as S
import Data.Map (Map())
import qualified Data.Map as M
import Data.List (List(Cons, Nil))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (foldr)
import Data.String (take)

data TreeSpec k v = TreeSpec
  { key :: k,
    value :: v,
    children :: Set k }

instance showTreeSpec :: (Show k, Show v) => Show (TreeSpec k v) where
  show (TreeSpec s) =
    "( " ++ (take 7 <<< show) s.key ++
    " " ++ show s.value ++
    " " ++ show (map (take 7 <<< show) $ S.toList s.children) ++ ")"

make :: forall v. (Show v) =>
        v ->
        Set String ->
        TreeSpec String v

make content children = TreeSpec
  { key: T.hash content children
  , value: content
  , children: children }

toSpec :: forall k v. (Ord k) =>
          Tree k v ->
          TreeSpec k v

toSpec tree = TreeSpec
  { key: T.key tree
  , value: T.value tree
  , children: foldr (\tree result -> S.insert (T.key tree) result)
                S.empty (T.children tree) }

toTree :: forall k v. (Ord k) =>
          k ->
          Map k (TreeSpec k v) ->
          Maybe (Tree k v)

toTree root specs =
  resolve root

  where resolve key = case M.lookup key specs of
          Just (TreeSpec s) -> case iterate (S.toList s.children) S.empty of
            Just trees -> Just $ T.tree s.key s.value trees
            Nothing -> Nothing
          Nothing -> Nothing
          
        iterate (Cons key keys) trees = case resolve key of
          Just tree -> iterate keys $ S.insert tree trees
          Nothing -> Nothing
          
        iterate Nil trees = Just trees

key (TreeSpec s) = s.key
