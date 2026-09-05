{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | The opposite category of a given category denoted C^op, where all arrows are reversed
module Ibis.Category.Presheaf.Opposite
  ( Dual (..)
  , OpArrow
  ) where

import Control.Category
import Data.Kind (Type)
import Prelude hiding (id, (.))

import Ibis.Category.Presheaf.Arrow (Arrow)

-- | Dual wraps arrows @arr a b@ in a category @k@ to represent the opposite category,
-- reversing the direction of morphisms, yielding @arr b a@
newtype Dual (arr :: k -> k -> Type) (a :: k) (b :: k) = Dual {runDual :: arr b a}

-- | An arrow in C^op, the opposite category of C, where objects are of type 'obj'
--
-- Represented as the dual of an @Arrow@ in the original category C
type OpArrow obj u v = Dual (Arrow obj) u v

instance (Category k) => Category (Dual k) where
  id = Dual id
  Dual g . Dual f = Dual (f . g)
