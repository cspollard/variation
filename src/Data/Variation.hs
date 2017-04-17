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
import qualified Data.IntMap          as IM
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
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
    } deriving (Generic, Show, Functor, Foldable, Traversable)

instance (NFData a, NFData (m a)) => NFData (Variations m a) where

makeLenses ''Variations

instance (Serialize (m a), Serialize a) => Serialize (Variations m a)

type instance Index (Variations m a) = Index (m a)
type instance IxValue (Variations m a) = IxValue (m a)


instance (Apply m, SMonoid m) => Applicative (Variations m) where
  pure = flip Variations sempty
  {-# INLINABLE pure #-}

  Variations f fs <*> Variations x xs =
    Variations (f x) ((fs <.> xs) `sappend` fmap f xs `sappend` fmap ($ x) fs)
  {-# INLINABLE (<*>) #-}


instance (Bind m, SMonoid m) => Monad (Variations m) where
  return = pure
  {-# INLINABLE return #-}

  v >>= f =
    let Variations (Variations n nm) mm = f <$> v
        mn = view nominal <$> mm
        mm' = view variations <$> mm
    in Variations n (join mm' `sappend` nm)
  {-# INLINABLE (>>=) #-}

--
-- how we join variations:
-- nominal -> nominal
-- if we have "on-diagonal" elements of mm, use them
-- else use nominal <*> varied from mm
-- else use varied <*> nominal
-- joinV :: AlignWithKey m => Variations m (Variations m a) -> Variations m a
-- joinV (Variations (Variations n m) mm) =
--   Variations n $ alignWithKey f mm m
--   where
--     f k = these
-- {-# INLINABLE joinV #-}
