{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}


module Data.Variation
  ( VariationT(..), variation, variationT
  , Variation, runVariation
  , nominal, variations
  , module X
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens                hiding ((<.>))
import qualified Control.Monad               as M (join)
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
  deriving (Generic, Functor, Foldable, Traversable)

type Variation f = VariationT f Identity

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

  VariationT f <*> VariationT x = VariationT $ g <$> f <*> x
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

  VariationT x >>= f = VariationT $ do
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


instance (MF.MonadFail m, Traversable f, Bind f, SMonoid f)
  => MF.MonadFail (VariationT f m) where
  fail = lift . MF.fail


instance MFunctor (VariationT f) where
  hoist f = VariationT . f . runVariationT

-- instance (SMonoid f, Bind f, Traversable f) => MMonad (VariationT f) where
--   embed f (VariationT v) = undefined

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


-- class MComonad t where
--   unembed :: Functor m => t m a -> m (t Identity a)
--
-- instance MComonad (VariationT f) where
--   unembed (VariationT x) = VariationT . Identity <$> x
--
-- instance MComonad (WriterT w) where
--   unembed (WriterT x) = WriterT . Identity <$> x
--
-- instance MComonad MaybeT where
--   unembed (MaybeT x) = MaybeT . Identity <$> x
--
--
-- class Asdf f where
--   asdf :: (MonadTrans g, Monad m, Monad (g m)) => g m (f m a) -> f (g m) a
--
-- instance Asdf MaybeT where
--   asdf x = MaybeT $ M.join $ lift . runMaybeT <$> x
--
--
-- instance (Functor (g Identity), Asdf f, MonadTrans g, Monad (g Identity), MComonad f, MComonad g) => MComonad (ComposeT f g) where
--   unembed = fmap (ComposeT . asdf) . unembed . unembed . getComposeT
