{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Core kernel for Ibis.
This module defines the custom categories, functors, presheaves and constructs related
to Ibis' categorical semantics.
-}
module Ibis.Kernel
  ( -- * Categories and Functors
    Arrow (..)
  , Dual (..)
  , OpArrow
  , KFunctor (..)

    -- * Presheaves and Sections
  , Presheaf (..)
  , Section (..)
  , GluedSection (..)
  , pullback
  , extend
  , mkPresheaf
  )
where

import Control.Category
import Data.Kind (Type)
import Prelude hiding (id, (.))

newtype Dual (arr :: k -> k -> Type) (a :: k) (b :: k) = Dual {runDual :: arr b a}

-- | An arrow in C^op, the opposite category of C, where objects are of type 'obj'
type OpArrow obj u v = Dual (Arrow obj) u v

instance (Category k) => Category (Dual k) where
  id = Dual id
  Dual g . Dual f = Dual (f . g)

-- | An arrow in a category where objects are of type 'obj'
data Arrow obj (u :: obj) (v :: obj) where
  -- The identity arrow for an object 'u' in a category
  Id :: Arrow obj u u
  -- A named arrow in a category, representing a morphism from object 'u' to object 'v'
  Path :: String -> Arrow obj u v
  -- Composition of two arrows: if f: u -> v and g: v -> w, then g . f: u -> w
  Comp :: Arrow obj v w -> Arrow obj u v -> Arrow obj u w

-- | A functor from a category 'k' to the category of Haskell types (Hask).
class (Category k) => KFunctor k f where
  -- | Maps a morphism in 'k' to a standard Haskell function between section/payload types.
  fmapK :: k v u -> f v -> f u

-- | A contravariant functor from a category 'k' to the category of Haskell types (Hask).
class (Category k) => KContravariant k f where
  contramapK :: k v u -> f v -> f u

-- | The Left Kan Extension (Lan_K F) of a functor 'f' along an embedding/morphism 'k'.
--
-- Given K : C -> D and F : C -> E, Lan k f d constructs the value of the extended
-- functor at target object 'd' in D.
data Lan arr f u where
  -- arr c u : Morphism in the category connecting the hidden domain object 'c' to target object 'u'
  -- f c     : The original functor F evaluated at the domain object 'c'
  -- c       : The existential intermediate object (coend index)
  Lan :: arr c u -> f c -> Lan arr f u

---------------------------------------------
-- CATEGORY INSTANCES
----------------------------------------------

instance Category (Arrow obj) where
  -- Identity arrows
  id = Id
  Id . f = f
  g . Id = g
  -- Composition of arrows
  g . f = Comp g f

instance (Category k) => KFunctor k (Lan k f) where
  fmapK h (Lan kc fc) = Lan (h . kc) fc

instance KContravariant (Arrow obj) (Section val) where
  contramapK Id secV = secV
  contramapK (Comp g f) secV =
    let secMid = contramapK f secV -- f : u -> v1, g : v1 -> v. Pulls secV to secMid over v1
     in contramapK g secMid -- pulls secMid to sec over u
  contramapK (Path name) secV = Restrict (Path name) secV

----------------------------------------------

----------------------------------------------
-- PRESHEAVES AND SECTIONS
----------------------------------------------

-- | A section of a presheaf over an object 'u' in a category
data Section val (u :: obj) where
  Base :: val -> Section val u
  Restrict
    :: Arrow obj u v
    -> Section val v
    -> Section val u

-- | A glued section of two sections over a binary cover (U ∪ V)
data GluedSection val (u :: obj) (v :: obj) where
  Glue
    :: Section val u
    -> Section val v
    -> GluedSection val u v

-- | A presheaf over a category 'k' with values of type 's' is a contravariant functor from
-- 'k' to 's' (the category of sets, or set-like structures)
data Presheaf k s where
  Presheaf
    :: { restrict :: forall u v. Dual k u v -> s v -> s u
       }
    -> Presheaf k s

-- | Pullback a section of a presheaf 'p' along an arrow in the category.
-- Given a presheaf 'p', an arrow 'arrow' from 'v' to 'u', and a section 'sec' over 'v', this function
-- returns a section over 'u'.
pullback :: Presheaf k s -> k v u -> s v -> s u
pullback p arrow sec = restrict p (Dual arrow) sec

-- | Extend a section of a presheaf along a left Kan extension.
extend :: Presheaf k s -> Lan k s u -> s u
extend p (Lan arrow fc) = restrict p (Dual arrow) fc

-- | Construct a presheaf of sections over a category of arrows and a given value type for the sections.
mkPresheaf :: Presheaf (Arrow obj) (Section val)
mkPresheaf = Presheaf{restrict = \(Dual arr) secV -> contramapK arr secV}