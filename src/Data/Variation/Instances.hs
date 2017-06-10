{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Data.Variation.Instances where


import           Data.Functor.Classes
import qualified Data.Map.Strict      as M

instance Show2 M.Map where
  liftShowsPrec2 spk slk spv slv d m =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (M.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (M.Map k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
