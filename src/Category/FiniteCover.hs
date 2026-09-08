{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Defines a finite cover over a spatial index object in a category, along with covering arrows
-- that represent the inclusion of subobjects or covers into the target object.
module Category.FiniteCover where

import Category.Presheaf.Arrow (Arrow)
import Data.Kind (Type)
import Data.Proxy (Proxy)

-- | A finite cover over a spatial index object 'c' in a category 'cat'
type FiniteCover :: forall cat -> cat -> Type

-- | A covering arrow into a spatial index object 'c' in a category 'cat'
data CoveringArrow cat (c :: cat) where
  CoveringArrow :: Arrow cat d c -> CoveringArrow cat c

data FiniteCover cat (c :: cat) where
  FiniteCover
    :: { coverObject :: Proxy c
       -- ^ Proxy tag for non-Hask kind index 'c'
       , depth :: Int
       -- ^ Spatial render distance / depth horizon
       , coveringArrows :: [CoveringArrow cat c]
       -- ^ Explicit list of covering arrows into 'c'
       }
    -> FiniteCover cat c