{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}


module Data.Variation
  ( VariationT, variationT, runVariationT
  , Variation, variation, runVariation
  , nominal, variations, getNominal, getVariations
  , module X
  ) where

import           Control.Applicative
import           Control.DeepSeq
import qualified Control.Monad             as M (join)
import           Control.Monad.Catch
import qualified Control.Monad.Fail        as MF
import           Control.Monad.Morph
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Data.Functor.Apply
import           Data.Functor.Bind
import           Data.Functor.Classes
import           Data.Functor.Identity
import           Data.Semigroup
import           Data.Serialize            (Serialize)
import           Data.SMonoid
import           Data.Variation.Instances  as X
import           GHC.Generics


-- strict tuple
data Pair f a = Pair !a !(f a)
  deriving Generic

instance (NFData a, NFData (f a)) => NFData (Pair f a)

instance (Serialize a, Serialize (f a)) => Serialize (Pair f a) where

instance Functor f => Functor (Pair f) where
  fmap f (Pair x xs) = Pair (f x) (fmap f xs)


instance Foldable f => Foldable (Pair f) where
  foldMap f (Pair x xs) = f x `mappend` foldMap f xs


instance Traversable f => Traversable (Pair f) where
  traverse f (Pair x xs) = Pair <$> f x <*> traverse f xs


fstP :: Pair f a -> a
fstP (Pair x _) = x

sndP :: Pair f a -> f a
sndP (Pair _ xs) = xs


newtype VariationT f m a =
  VariationT { unVT :: m (Pair f a) }
  deriving Generic

instance
  (NFData a, NFData (f a), NFData (m (Pair f a)))
  => NFData (VariationT f m a)

instance (Functor f, Functor m) => Functor (VariationT f m) where
  fmap f (VariationT mp) = VariationT $ (fmap.fmap) f mp

instance (Foldable f, Foldable m) => Foldable (VariationT f m) where
  foldMap f (VariationT mp) = foldMap (foldMap f) mp

instance (Traversable f, Traversable m) => Traversable (VariationT f m) where
  traverse f (VariationT mp) = VariationT <$> (traverse.traverse) f mp

instance (Serialize (m (Pair f a))) => Serialize (VariationT f m a) where


type Variation f = VariationT f Identity

runVariation :: Variation f a -> (a, f a)
runVariation (VariationT (Identity (Pair x xs))) = (x, xs)


runVariationT :: Functor m => VariationT f m a -> m (Variation f a)
runVariationT (VariationT mp) = fmap (VariationT . Identity) mp


instance Show1 f => Show1 (Pair f) where
  liftShowsPrec f g n (Pair x xs) =
    showsBinaryWith f (liftShowsPrec f g) "Pair" n x xs


instance (Show1 f, Show1 m) => Show1 (VariationT f m) where
  liftShowsPrec f g n (VariationT mv) = showsUnaryWith go "VariationT" n mv
    where
      go = liftShowsPrec (liftShowsPrec f g) (liftShowList f g)


instance (Show1 f, Show a) => Show (Pair f a) where
  showsPrec = showsPrec1


instance (Show1 f, Show1 m, Show a) => Show (VariationT f m a) where
  showsPrec = showsPrec1


getNominal :: Functor m => VariationT f m a -> m a
getNominal = fmap fstP . unVT


getVariations :: Functor m => VariationT f m a -> m (f a)
getVariations = fmap sndP . unVT


nominal :: Monad m => (a -> m a) -> VariationT f m a -> VariationT f m a
nominal f (VariationT mp) = VariationT $ do
  Pair x xs <- mp
  x' <- f x
  return $ Pair x' xs


variations :: Monad m => (f a -> m (f a)) -> VariationT f m a -> VariationT f m a
variations f (VariationT mp) = VariationT $ do
  Pair x xs <- mp
  xs' <- f xs
  return $ Pair x xs'


instance (Apply f, SMonoid f) => Applicative (Pair f) where
  pure = flip Pair sempty
  Pair f fs <*> Pair x xs =
    Pair
      (f x)
      ((fs <.> xs) `sappend` (f <$> xs) `sappend` (($ x) <$> fs))


instance (Apply f, SMonoid f, Applicative m) => Applicative (VariationT f m) where
  pure = VariationT . pure . pure

  VariationT f <*> VariationT x = VariationT $ liftA2 (<*>) f x


instance (Traversable f, Bind f, SMonoid f, Monad m) => Monad (VariationT f m) where
  return = pure

  VariationT mx >>= f = VariationT $ do
    (Pair vfmb fvfmb) <- fmap f <$> mx
    (Pair nom fv) <- unVT vfmb
    (Pair nv ffb) <- unVT $ sequence fvfmb
    return $ Pair nom (join ffb `sappend` nv `sappend` fv)


instance (Traversable f, Bind f, SMonoid f) => MonadTrans (VariationT f) where
  lift = VariationT . fmap pure


instance
  (MonadIO m, Traversable f, Bind f, SMonoid f)
  => MonadIO (VariationT f m) where
  liftIO = lift . liftIO


instance (MonadIO m, Traversable f, Bind f, SMonoid f, MF.MonadFail m)
  => MF.MonadFail (VariationT f m) where
  fail = lift . fail


instance (MonadIO m, Traversable f, Bind f, SMonoid f, MonadThrow m)
  => MonadThrow (VariationT f m) where
  throwM = lift . throwM


instance (MonadIO m, Traversable f, Bind f, SMonoid f, MonadCatch m)
  => MonadCatch (VariationT f m) where
  catch (VariationT mx) f = VariationT $ catch mx (unVT . f)


instance
  (MonadIO m, Traversable f, Bind f, SMonoid f, MonadCatch m, MonadState s m)
  => MonadState s (VariationT f m) where
  get = lift get
  put = lift . put


instance MFunctor (VariationT f) where
  hoist f = VariationT . f . unVT


instance
  (Applicative m, Apply f, SMonoid f, Semigroup a)
  => Semigroup (VariationT f m a) where
  (<>) = liftA2 (<>)


instance
  (Applicative m, Monoid a, SMonoid f, Apply f)
  => Monoid (VariationT f m a) where
  mempty = pure mempty
  mappend = liftA2 mappend


variation :: Applicative m => a -> f a -> VariationT f m a
variation n vs = variationT (pure n) (pure vs)


variationT :: Applicative m => m a -> m (f a) -> VariationT f m a
variationT n vs = VariationT $ liftA2 Pair n vs
