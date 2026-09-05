{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines an arrow for a presheaf, representing inclusion, composition and identity between
-- objects in a category.
module Ibis.Category.Presheaf.Arrow
  ( Arrow (..)
  , eqArrow
  ) where

import Control.Category
import Data.Type.Equality ((:~:) (Refl))
import Prelude hiding (id, (.))

-- | An arrow in a category where objects are of type 'obj'
data Arrow obj (u :: obj) (v :: obj) where
  -- The identity arrow for an object 'u' in a category
  Id :: Arrow obj u u
  -- Inclusion: u is a valid subobject/cover of v, an arrow from u -> v
  Inclusion :: Arrow obj u v
  -- Composition of two arrows: if f: u -> v and g: v -> w, then g . f: u -> w
  Comp :: Arrow obj v w -> Arrow obj u v -> Arrow obj u w

instance Category (Arrow obj) where
  id = Id
  Id . f = f
  g . Id = g
  g . f = Comp g f

-- Equality for an Arrow is defined by structural equality
instance Eq (Arrow obj u v) where
  a1 == a2 = case eqArrow a1 a2 of
    Just Refl -> True
    Nothing -> False

eqArrow :: Arrow obj u v -> Arrow obj u w -> Maybe (v :~: w)
eqArrow Id Id = Just Refl
eqArrow (Comp g1 f1) (Comp g2 f2) = do
  Refl <- eqArrow f1 f2
  Refl <- eqArrow g1 g2
  Just Refl
-- Two un-indexed Inclusion constructors into ambiguous targets v and w cannot prove v ~ w,
-- so they fail type equality
eqArrow _ _ = Nothing
