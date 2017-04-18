{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


module Data.Variation where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens         hiding ((<.>))
import           Data.Functor.Apply
import           Data.Functor.Bind
import           Data.Functor.Classes
import qualified Data.IntMap.Strict   as IM
import qualified Data.Map.Strict      as M
import           Data.Semigroup
import           Data.Serialize
import           GHC.Generics


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




data Variations m a =
  Variations
    { _nominal    :: !a
    , _variations :: !(m a)
    } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)


instance (NFData a, NFData (m a)) => NFData (Variations m a) where

instance (Serialize (m a), Serialize a) => Serialize (Variations m a)

makeLenses ''Variations

instance (Apply m, SMonoid m) => Applicative (Variations m) where
  pure = flip Variations sempty
  {-# INLINABLE pure #-}

  Variations f fs <*> Variations x xs =
    Variations (f x) ((fs <.> xs) `sappend` fmap f xs `sappend` fmap ($ x) fs)
  {-# INLINABLE (<*>) #-}


instance (Bind m, SMonoid m) => Monad (Variations m) where
  return = pure
  {-# INLINABLE return #-}

  -- TODO
  -- I'm not sure if mn or nm should be used here...
  -- surely there's 100% overlap?
  v >>= f =
    let Variations (Variations n nm) mm = f <$> v
        mn = view nominal <$> mm
        mm' = view variations <$> mm
    in Variations n (join mm' `sappend` nm `sappend` mn)
  {-# INLINABLE (>>=) #-}


instance (Semigroup a, SMonoid m, Apply m) => Semigroup (Variations m a) where
  (<>) = liftA2 (<>)

instance (Monoid a, SMonoid m, Apply m) => Monoid (Variations m a) where
  mempty = Variations mempty sempty
  mappend = liftA2 mappend

instance Show1 m => Show1 (Variations m) where
  liftShowsPrec sp sl d (Variations n m) =
    showsBinaryWith sp (liftShowsPrec sp sl) "Variations" d n m


instance Show2 M.Map where
  liftShowsPrec2 spk slk spv slv d m =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (M.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (M.Map k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
