module Data.Monoid1
  ( Unit1(..), Append1(..), Monoid1
  ) where

import           Control.Applicative (Const (..), WrappedMonad (..))
import qualified Data.IntMap.Strict  as IM
import qualified Data.Map            as M
import           Data.Monoid


-- | the class of containers with a "nil" element
class Unit1 m where
  empty1 :: m a

instance Unit1 [] where
  empty1 = mempty

instance Unit1 IM.IntMap where
  empty1 = mempty

instance Unit1 (M.Map k) where
  empty1 = M.empty

instance Monoid a => Unit1 (Const a) where
  empty1 = mempty

instance Unit1 First where
  empty1 = mempty

instance Unit1 Last where
  empty1 = mempty


-- | the class of containers that can be combined regardless of type they
-- contain
class Append1 m where

  append1 :: m a -> m a -> m a

instance Append1 [] where
  append1 = mappend

instance Append1 IM.IntMap where
  append1 = mappend

instance Ord k => Append1 (M.Map k) where
  append1 = mappend

instance Monoid a => Append1 (Const a) where
  append1 = mappend

instance Append1 First where
  append1 = mappend

instance Append1 Last where
  append1 = mappend


-- | the class of containers that form a 'Monoid' regardless of the type they
-- contain
class (Unit1 m, Append1 m) => Monoid1 m where

instance Monoid1 [] where

instance Monoid1 IM.IntMap where

instance Ord k => Monoid1 (M.Map k) where

instance Monoid a => Monoid1 (Const a) where

instance Monoid1 First where

instance Monoid1 Last where


instance Unit1 m => Unit1 (WrappedMonad m) where
  empty1 = WrapMonad empty1

instance Append1 m => Append1 (WrappedMonad m) where
  WrapMonad x `append1` WrapMonad y = WrapMonad $ x `append1` y

instance Monoid1 m => Monoid1 (WrappedMonad m) where
