{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}


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
import           Data.Semigroup
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
-- the 'Applicative' instance uses the 'Unit1' instance of @f@ to define pure
--
-- > pure x = Variation x empty1
--
-- and the 'Bind' and 'Append1' instances of @f@ to define '<*>'
--
-- > Variation f fs <*> Variation x xs =
-- >   Variation
-- >     (f x)
-- >     ((fs <.> xs) `append1` (f <$> xs) `append1` (($ x) <$> fs))
--
-- the 'Monad' instance uses the 'Bind' instance of @f@ ('join') to collapse
-- collections of type @f (f a)@
--
-- > joinV :: (Bind f, Monoid1 f) => Variation f (Variation f a) -> Variation f a
-- > joinV (Variation (Variation nn nv) v) =
-- >   let vv = _variations <$> v
-- >       vn = _nominal <$> v
-- >   in Variation nn $ join vv `append1` vn `append1` nv
--
-- other useful instances:
--
-- > instance Append1 f => Semigroup (Variation f a) where
-- >   (<>) = append1
--
-- > instance (Monoid a, Monoid1 f) => Monoid (Variation f a) where
-- >   mempty = Variation mempty empty1
-- >   mappend = (<>)



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


-- some thoughts:
-- the requirements of Apply f and Monoid1 f appear to be related to
-- the Align typeclass in the "these" package.
-- there's something going on there.

-- what if we want to use ZipList here? it seems there is no monad instance
-- for ZipList, which makes it difficult to use (Variation ZipList a)...

instance Align f => Applicative (Variation f) where
  pure = flip Variation nil
  Variation f fs <*> Variation x xs =
    Variation
      (f x)
      (alignWith comb fs xs)

    where
      comb (This g)    = g x
      comb (That y)    = f y
      comb (These g y) = g y


instance (Trace f, Align f) => Monad (Variation f) where
  return = pure
  Variation x xs >>= f =
    let Variation nn nv = f x
        v = f <$> xs
        vv = _variations <$> v
        vn = _nominal <$> v
    in Variation nn $ alignWith comb (diagonal vv) $ alignWith comb nv vn

    where
      comb (This y)    = y
      comb (That y)    = y
      comb (These y _) = y


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
