
module Data.SMonoid where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M

class SUnit sm where
  sempty :: sm a

instance SUnit [] where
  sempty = []

instance SUnit IM.IntMap where
  sempty = IM.empty

instance SUnit (M.Map k) where
  sempty = M.empty

class SAppend sm where
  sappend :: sm a -> sm a -> sm a

instance SAppend [] where
  sappend = (++)

instance SAppend IM.IntMap where
  sappend = IM.union

instance Ord k => SAppend (M.Map k) where
  sappend = M.union

class (SUnit sm, SAppend sm) => SMonoid sm where

instance SMonoid [] where

instance SMonoid IM.IntMap where

instance Ord k => SMonoid (M.Map k) where
