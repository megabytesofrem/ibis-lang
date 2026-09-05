{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines a presheaf - a contravariant functor from a category to the category of sets (or set-like structures)
-- along with restriction operations over sections within the presheaf, and sheaf gluing axioms for combining sections over covers.
module Ibis.Category.Presheaf.Type
  ( -- * Custom functor and contravariant functor classes
    KFunctor (..)
  , KContravariant (..)

    -- * Left Kan Extension
  , Lan (..)

    -- * Sections and Glued Sections
  , Section (..)
  , GluedSection (..)

    -- * Presheaf operations
  , Presheaf (..)
  , pullback
  , extend
  , glue
  , mkPresheaf
  ) where

import Control.Category
import Data.Type.Equality ((:~:) (Refl))

import Prelude hiding (id, (.))

import Ibis.Category.Presheaf.Arrow (Arrow (..), eqArrow)
import Ibis.Category.Presheaf.Opposite (Dual (..))

-- | A functor from a category 'k' to the category of Haskell types (Hask).
-- Data.Functor is too specialized so we define KFunctor - a functor over arbitrary categories.
class (Category k) => KFunctor k f where
  -- | Maps a morphism in 'k' to a standard Haskell function between section/payload types.
  fmapK :: k v u -> f v -> f u

-- | A contravariant functor from a category 'k' to the category of Haskell types (Hask).
-- Data.Functor.Contravariant is too specialized so we define KContravariant - a contravariant functor over arbitrary categories.
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

instance (Category k) => KFunctor k (Lan k f) where
  fmapK h (Lan kc fc) = Lan (h . kc) fc

instance KContravariant (Arrow obj) (Section val) where
  contramapK Id secV = secV
  contramapK Inclusion secV = Restrict Inclusion secV
  contramapK (Comp g f) secV =
    let secMid = contramapK f secV
     in contramapK g secMid

instance (KContravariant (Arrow obj) (Section val)) => KContravariant (Dual (Arrow obj)) (Section val) where
  contramapK (Dual arr) sec = Restrict arr sec

instance (Eq val) => Eq (Section val u) where
  Base v1 == Base v2 = v1 == v2
  Restrict arr1 sec1 == Restrict arr2 sec2 =
    case eqArrow arr1 arr2 of
      Just Refl -> sec1 == sec2
      Nothing -> False
  _ == _ = False

instance (Eq val) => Eq (GluedSection val u v) where
  Glue sec1u sec1v == Glue sec2u sec2v = sec1u == sec2u && sec1v == sec2v

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
pullback :: Presheaf k s -> k v u -> s v -> s u
pullback p arrow sec = restrict p (Dual arrow) sec

-- | Extend a section of a presheaf along a left Kan extension.
extend :: Presheaf k s -> Lan k s u -> s u
extend p (Lan arrow fc) = restrict p (Dual arrow) fc

-- | Sheaf gluing axiom
glue
  :: (Eq val)
  => Arrow obj w u
  -> Arrow obj w v
  -> Section val u
  -> Section val v
  -> Maybe (GluedSection val u v)
glue arrU arrV secU secV =
  let resU = Restrict arrU secU
      resV = Restrict arrV secV
   in if resU == resV
        then Just (Glue secU secV)
        else Nothing

-- | Construct a presheaf of sections over a category of arrows and a given value type for the sections.
mkPresheaf :: Presheaf (Arrow obj) (Section val)
mkPresheaf = Presheaf{restrict = \(Dual arr) secV -> contramapK arr secV}
