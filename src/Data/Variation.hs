{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}


-- A lot of this was inspired by
-- https://hackage.haskell.org/package/total-map-0.0.6/

module Data.Variation where

import           Control.Applicative
import           Control.Lens
import           Data.Functor.Classes
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup
import           Data.Serialize
import           GHC.Generics


data Variations k a =
  Variations
    { _nominal    :: !a
    , _variations :: !(M.Map k a)
    } deriving (Generic, Show)

makeLenses ''Variations

variationsToMap :: Ord k => k -> Variations k a -> M.Map k a
variationsToMap nomname (Variations nom def) = M.insert nomname nom def

instance (Ord k, Serialize k, Serialize a) => Serialize (Variations k a)

type instance Index (Variations k a) = k
type instance IxValue (Variations k a) = a

instance Ord k => Ixed (Variations k a) where
  ix k = variations.ix k
  {-# INLINABLE ix #-}

instance Ord k => At (Variations k a) where
  at k = variations.at k
  {-# INLINABLE at #-}

instance Ord k => Functor (Variations k) where
  fmap f (Variations n m) = Variations (f n) (fmap f m)
  {-# INLINABLE fmap #-}


instance Show k => Show1 (Variations k) where
  liftShowsPrec sp sl d (Variations n m) =
    showsBinaryWith sp (liftShowsPrec sp sl) "Variations" d n m

instance (Ord k, Semigroup a) => Semigroup (Variations k a) where
  (<>) = liftA2 (<>)
  {-# INLINABLE (<>) #-}

instance (Ord k, Monoid a) => Monoid (Variations k a) where
  mempty = pure mempty
  mappend = liftA2 mappend
  {-# INLINABLE mempty #-}
  {-# INLINABLE mappend #-}



instance Ord k => Applicative (Variations k) where
  pure = flip Variations M.empty
  -- if the same variation appears in both maps
  -- then we apply the function to the corresponding value
  -- otherwise "fill in" with the nominal value
  Variations f fs <*> Variations x xs =
    Variations
      (f x)
      ( M.mergeWithKey
        (\_ g -> Just . g)
        (fmap ($ x))
        (fmap f)
        fs
        xs
      )
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance Ord k => Monad (Variations k) where
  return = pure
  m >>= f = joinV (f <$> m)
  {-# INLINABLE return #-}
  {-# INLINABLE (>>=) #-}

(!) :: Ord k => Variations k a -> k -> a
(!) (Variations n m) = fromMaybe n . flip M.lookup m
{-# INLINABLE (!) #-}

-- how we join variations:
-- nominal -> nominal
-- if we have "on-diagonal" elements of mm, use them
-- else use nominal <*> varied from mm
-- else use varied <*> nominal
joinV :: Ord k => Variations k (Variations k v) -> Variations k v
joinV (Variations (Variations n m) mm) =
  Variations n (M.mapWithKey (flip (!)) mm `M.union` m)
{-# INLINABLE joinV #-}

instance Ord k => Foldable (Variations k) where
  foldMap f (Variations n m) = f n `mappend` foldMap f m
  {-# INLINABLE foldMap #-}

instance Ord k => Traversable (Variations k) where
  traverse f (Variations n m) = Variations <$> f n <*> traverse f m
  {-# INLINABLE traverse #-}

instance Show2 M.Map where
  liftShowsPrec2 spk slk spv slv d m =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (M.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (M.Map k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
