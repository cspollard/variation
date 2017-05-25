{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Variation
  ( VariationT(..), variation, variationT
  , Variation, runVariation
  , nominal, variations, getNominal, getVariations
  , module X
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens                hiding ((<.>))
import qualified Control.Monad               as M (join)
import           Control.Monad.Catch
import qualified Control.Monad.Fail          as MF
import           Control.Monad.Morph
import           Control.Monad.State         hiding (join)
import           Control.Monad.Trans
import           Control.Monad.Trans.Compose
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer        hiding (join, (<>))
import           Data.Functor.Apply
import           Data.Functor.Bind
import           Data.Functor.Classes
import           Data.Functor.Product        (Product (Pair))
import qualified Data.Functor.Product        as P
import           Data.Semigroup
import           Data.Serialize
import           Data.SMonoid
import           Data.Variation.Instances    as X
import           GHC.Generics


newtype VariationT f m a =
  VariationT { runVariationT :: m (P.Product Identity f a) }
  deriving Generic

type Variation f = VariationT f Identity

forceProd :: P.Product f g a -> P.Product f g a
forceProd (Pair fa ga) = fa `seq` ga `seq` Pair fa ga

instance (Functor m, Functor f) => Functor (VariationT f m) where
  fmap f (VariationT mx) = VariationT $ forceProd . fmap f <$> mx

instance (Foldable m, Foldable f) => Foldable (VariationT f m) where
  foldMap f (VariationT mx) = foldMap (foldMap f) mx

instance (Traversable m, Traversable f) => Traversable (VariationT f m) where
  traverse f (VariationT mx) = VariationT <$> traverse (fmap forceProd . traverse f) mx

instance (Show1 f, Show1 m) => Show1 (VariationT f m) where
  liftShowsPrec f g n (VariationT mv) =
    showsUnaryWith
      (liftShowsPrec (liftShowsPrec f g) (liftShowList f g))
      "VariationT"
      n
      mv

instance (Show1 f, Show1 m, Show a) => Show (VariationT f m a) where
  showsPrec = showsPrec1

runVariation :: VariationT f Identity a -> P.Product Identity f a
runVariation = runIdentity . runVariationT

fstP :: P.Product f g a -> f a
fstP (Pair x _) = x

sndP :: P.Product f g a -> g a
sndP (Pair _ x) = x

getNominal :: Functor m => VariationT f m a -> m a
getNominal = fmap (runIdentity . fstP) . runVariationT

getVariations :: Functor m => VariationT f m a -> m (f a)
getVariations = fmap sndP . runVariationT

nominal :: Monad m => (a -> m a) -> VariationT f m a -> VariationT f m a
nominal f (VariationT mp) = VariationT $ do
  Pair (Identity x) y <- mp
  x' <- f x
  return $ Pair (Identity x') y

variations :: Monad m => (f a -> m (f a)) -> VariationT f m a -> VariationT f m a
variations f (VariationT mp) = VariationT $ do
  Pair x y <- mp
  y' <- f y
  return $ Pair x y'


instance (Apply f, SMonoid f, Applicative m) => Applicative (VariationT f m) where
  pure = VariationT . pure . flip Pair sempty . pure

  VariationT f <*> VariationT x = VariationT . fmap forceProd $ g <$> f <*> x
    where
      g (Pair f1 f2) (Pair x1 x2) =
        Pair
          (f1 <.> x1)
          ( (f2 <.> x2)
            `sappend` fmap (runIdentity f1) x2
            `sappend` fmap ($ runIdentity x1) f2
          )

instance (Traversable f, Bind f, SMonoid f, Monad m) => Monad (VariationT f m) where
  return = pure

  VariationT x >>= f = VariationT . fmap forceProd $ do
    -- fmap f <$> x :: m (Product Identity f (VariationT f m a))
    (Pair (Identity vfmb) fvfmb) <- fmap f <$> x
    (Pair nom fv) <- runVariationT vfmb
    (Pair (Identity nv) ffb) <- runVariationT $ sequence fvfmb
    return . Pair nom $ join ffb `sappend` nv `sappend` fv


instance (Traversable f, Bind f, SMonoid f) => MonadTrans (VariationT f) where
  lift = VariationT . fmap (flip Pair sempty . Identity)


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
  catch (VariationT mx) f = VariationT $ catch mx (runVariationT . f)


instance MFunctor (VariationT f) where
  hoist f = VariationT . f . runVariationT

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
variationT n vs = VariationT $ Pair <$> (Identity <$> n) <*> vs
