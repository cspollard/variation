{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}


module Data.Variation
  (
  -- * Variation
    Variation(..)

  -- * Lenses
    , nominal, variations
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Align
import           Data.Functor.Classes
import           Data.Serialize       (Serialize)
import           Data.These
import           GHC.Generics
import           Linear.Matrix        (Trace (..))


-- | the variation type contains
--
--   [@_nominal@] : a nominal value that will always exist
--
--   [@_variations@] : alternative values which are held inside a container of
--                     type @f@
--
-- it is strict in both arguments.
--
-- the 'Applicative' instance uses the 'Align' instance of @f@ to define 'pure'
-- and '<*>':
--
-- > instance Align f => Applicative (Variation f) where
-- >   pure = flip Variation nil
-- >   Variation f fs <*> Variation x xs =
-- >     Variation
-- >       (f x)
-- >       (alignWith comb fs xs)
-- >
-- >     where
-- >       comb = these ($ x) f ($)
--
-- the 'Monad' instance uses the 'Trace' instance of @f@ ('diagonal') to
-- collapse collections of type @f (f a)@
--
-- > Variation x xs >>= f =
-- >   let Variation nn nv = f x
-- >       v = f <$> xs
-- >       vv = _variations <$> v
-- >       vn = _nominal <$> v
-- >   in Variation nn $ alignWith comb (diagonal vv) $ alignWith comb nv vn
-- >
-- >     where
-- >       comb = these ($ x) f ($)

data Variation f a =
  Variation
    { _nominal    :: !a
    , _variations :: !(f a)
    } deriving (Generic, Functor, Foldable, Traversable)


nominal :: Functor f => (a -> f a) -> Variation t a -> f (Variation t a)
nominal f (Variation n v) = flip Variation v <$> f n

variations :: Functor f => (t a -> f (t a)) -> Variation t a -> f (Variation t a)
variations f (Variation n v) = Variation n <$> f v


instance (NFData a, NFData (f a)) => NFData (Variation f a)

instance (Serialize a, Serialize (f a)) => Serialize (Variation f a) where


instance Align f => Applicative (Variation f) where
  pure = flip Variation nil
  Variation f fs <*> Variation x xs =
    Variation
      (f x)
      (alignWith comb fs xs)

    where
      comb = these ($ x) f ($)


instance (Trace f, Align f) => Monad (Variation f) where
  return = pure
  Variation x xs >>= f =
    let Variation nn nv = f x
        v = f <$> xs
        vv = _variations <$> v
        vn = _nominal <$> v
    in Variation nn $ alignWith comb (diagonal vv) $ alignWith comb nv vn

    where
      comb = these id id const


instance (Align f, Semigroup a) => Semigroup (Variation f a) where
  (<>) = liftA2 (<>)


instance (Align f, Monoid a) => Monoid (Variation f a) where
  mempty = pure mempty
  mappend = liftA2 mappend


instance Show1 f => Show1 (Variation f) where
  liftShowsPrec f g n (Variation x xs) =
    showsBinaryWith f (liftShowsPrec f g) "Variation" n x xs


instance (Show1 f, Show a) => Show (Variation f a) where
  showsPrec = showsPrec1
