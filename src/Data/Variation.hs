{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


-- A lot of this was inspired by
-- https://hackage.haskell.org/package/total-map-0.0.6/

module Data.Variation where

import Data.Align
import Data.These
import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Functor.Classes
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup
import           Data.Serialize
import           GHC.Generics


data Variations m a =
  Variations
    { _nominal    :: !a
    , _variations :: !(m a)
    } deriving (Generic, Show)

instance (NFData a, NFData (m a)) => NFData (Variations m a) where

makeLenses ''Variations

-- variationsToMap :: Ord k => k -> Variations m a -> M.Map m a
-- variationsToMap nomname (Variations nom def) = M.insert nomname nom def

instance (Serialize (m a), Serialize a) => Serialize (Variations m a)

type instance Index (Variations m a) = Index (m a)
type instance IxValue (Variations m a) = IxValue (m a)

-- TODO
-- think about this...
-- do we really want to return Nothing or the nominal?
-- instance Ixed (m a) => Ixed (Variations m a) where
--   ix k = variations.ix k
--   {-# INLINABLE ix #-}
--
-- instance Ord k => At (Variations m a) where
--   at k = variations.at k
--   {-# INLINABLE at #-}

instance Functor m => Functor (Variations m) where
  fmap f (Variations n m) = Variations (f n) (fmap f m)
  {-# INLINABLE fmap #-}

instance Align m => Applicative (Variations m) where
  pure = flip Variations nil
  Variations f fs <*> Variations x xs =
    Variations (f x) (alignWith g fs xs)
    where
      g = these ($ x) f ($)
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

-- TODO
-- HERE
-- I think we can encode the "join" in a one-liner with align/these
-- not quite sure how yet, though
-- instance Align m => Monad (Variations m) where
--   return = pure
--   vx >>= f = joinV . fmap f $ vx


-- joinV :: Align m => Variations m (Variations m a) -> Variations m a
-- joinV (Variations (Variations x mx) mvx) =
--   Variations x . view nominal $ sequenceA mvx

-- (!) :: Ord k => Variations m a -> k -> a
-- (!) (Variations n m) = fromMaybe n . flip M.lookup m
-- {-# INLINABLE (!) #-}

-- how we join variations:
-- nominal -> nominal
-- if we have "on-diagonal" elements of mm, use them
-- else use nominal <*> varied from mm
-- else use varied <*> nominal
-- joinV :: Ord k => Variations m (Variations m v) -> Variations m v
-- joinV (Variations (Variations n m) mm) =
--   Variations n (M.mapWithKey (flip (!)) mm `M.union` m)
-- {-# INLINABLE joinV #-}

-- instance Show1 m => Show1 (Variations m) where
--   liftShowsPrec sp sl d (Variations n m) =
--     showsBinaryWith sp (liftShowsPrec sp sl) "Variations" d n m
--
-- instance Semigroup a => Semigroup (Variations m a) where
--   (<>) = liftA2 (<>)
--   {-# INLINABLE (<>) #-}
--
-- instance (Ord k, Monoid a) => Monoid (Variations m a) where
--   mempty = pure mempty
--   mappend = liftA2 mappend
--   {-# INLINABLE mempty #-}
--   {-# INLINABLE mappend #-}




-- instance Ord k => Monad (Variations k) where
--   return = pure
--   m >>= f = joinV (f <$> m)
--   {-# INLINABLE return #-}
--   {-# INLINABLE (>>=) #-}

-- instance Ord k => Foldable (Variations k) where
--   foldMap f (Variations n m) = f n `mappend` foldMap f m
--   {-# INLINABLE foldMap #-}
--
-- instance Ord k => Traversable (Variations k) where
--   traverse f (Variations n m) = Variations <$> f n <*> traverse f m
--   {-# INLINABLE traverse #-}

instance Show2 M.Map where
  liftShowsPrec2 spk slk spv slv d m =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (M.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (M.Map k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
