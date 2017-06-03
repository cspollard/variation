{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}


module Data.Variation
  (Variation(..), nominal, variations
  , module X
  ) where

import           Control.DeepSeq
import           Control.Lens             hiding ((<.>))
import           Data.Functor.Apply
import           Data.Functor.Bind
import           Data.Functor.Classes
import           Data.Semigroup
import           Data.Serialize           (Serialize)
import           Data.SMonoid
import           Data.Variation.Instances as X ()
import           GHC.Generics


-- strict tuple
data Variation f a =
  Variation
    { _nominal    :: !a
    , _variations :: !(f a)
    } deriving Generic

makeLenses ''Variation

instance (NFData a, NFData (f a)) => NFData (Variation f a)

instance (Serialize a, Serialize (f a)) => Serialize (Variation f a) where

instance Functor f => Functor (Variation f) where
  fmap f (Variation x xs) = Variation (f x) (fmap f xs)

instance (Apply f, SMonoid f) => Applicative (Variation f) where
  pure = flip Variation sempty
  Variation f fs <*> Variation x xs =
    Variation
      (f x)
      ((fs <.> xs) `sappend` (f <$> xs) `sappend` (($ x) <$> fs))


joinV :: (Bind f, SMonoid f) => Variation f (Variation f a) -> Variation f a
joinV (Variation (Variation nn nv) v) =
  let vv = view variations <$> v
      vn = view nominal <$> v
  in Variation nn $ join vv `sappend` vn `sappend` nv

instance (Bind f, SMonoid f) => Monad (Variation f) where
  return = pure
  p >>= f = joinV $ f <$> p


instance Foldable f => Foldable (Variation f) where
  foldMap f (Variation x xs) = f x `mappend` foldMap f xs


instance Traversable f => Traversable (Variation f) where
  traverse f (Variation x xs) = Variation <$> f x <*> traverse f xs


instance SAppend f => SAppend (Variation f) where
  Variation x xs `sappend` Variation _ ys = Variation x (xs `sappend` ys)

instance SAppend f => Semigroup (Variation f a) where
  (<>) = sappend

instance (Monoid a, SMonoid f) => Monoid (Variation f a) where
  mempty = Variation mempty sempty
  mappend = (<>)

instance Show1 f => Show1 (Variation f) where
  liftShowsPrec f g n (Variation x xs) =
    showsBinaryWith f (liftShowsPrec f g) "Variation" n x xs


instance (Show1 f, Show a) => Show (Variation f a) where
  showsPrec = showsPrec1
