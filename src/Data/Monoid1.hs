
module Data.SMonoid where

import           Control.Applicative (WrappedMonad (..))
import           Data.Functor.Const
import qualified Data.IntMap.Strict  as IM
import qualified Data.Map.Strict     as M
import           Data.Semigroup

class SUnit sm where
  sempty :: sm a

instance SUnit [] where
  sempty = []

instance SUnit IM.IntMap where
  sempty = IM.empty

instance SUnit (M.Map k) where
  sempty = M.empty

instance Monoid a => SUnit (Const a) where
  sempty = mempty

class SAppend sm where
  sappend :: sm a -> sm a -> sm a

instance SAppend [] where
  sappend = (++)

instance SAppend IM.IntMap where
  sappend = IM.union

instance Ord k => SAppend (M.Map k) where
  sappend = M.union

instance Monoid a => SAppend (Const a) where
  sappend = mappend

class (SUnit sm, SAppend sm) => SMonoid sm where

instance SMonoid [] where

instance SMonoid IM.IntMap where

instance Ord k => SMonoid (M.Map k) where

instance Monoid a => SMonoid (Const a) where

instance SUnit m => SUnit (WrappedMonad m) where
  sempty = WrapMonad sempty

instance SAppend m => SAppend (WrappedMonad m) where
  WrapMonad x `sappend` WrapMonad y = WrapMonad $ x `sappend` y

instance SMonoid m => SMonoid (WrappedMonad m) where
