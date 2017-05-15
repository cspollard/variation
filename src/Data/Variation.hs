{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Data.Variation
  ( VariationT(..), Variations(..), nominal, variations, module X
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens             hiding ((<.>))
import qualified Control.Monad            as M (join)
import           Control.Monad.State      hiding (join)
import           Control.Monad.Trans
import           Control.Monad.Writer     hiding (join, (<>))
import           Data.Functor.Apply
import           Data.Functor.Bind
import           Data.Functor.Classes
import           Data.Semigroup
import           Data.Serialize
import           Data.SMonoid
import           Data.Variation.Instances as X
import           GHC.Generics


newtype VariationT f m a = VariationT { runVariationT :: m (Variations f a) }

instance (Functor f, Functor m) => Functor (VariationT f m) where
  fmap f = VariationT . (fmap.fmap) f . runVariationT

instance (Apply f, SMonoid f, Applicative m) => Applicative (VariationT f m) where
  pure = VariationT . pure . pure
  VariationT f <*> VariationT x = VariationT $ liftA2 (<*>) f x


instance
  (Traversable f, Bind f, SMonoid f, Monad m)
  => Monad (VariationT f m) where

  return = pure
  v >>= f = VariationT $ do
    x <- runVariationT v
    xs <- traverse (runVariationT . f) x
    return $ M.join xs

instance (Traversable f, Bind f, SMonoid f) => MonadTrans (VariationT f) where
  lift = VariationT . fmap pure


instance
  (MonadIO m, Traversable f, Bind f, SMonoid f)
  => MonadIO (VariationT f m) where

  liftIO = lift . liftIO



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
