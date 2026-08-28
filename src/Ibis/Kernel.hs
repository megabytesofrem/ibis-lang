{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ibis.Kernel where

import Control.Category
import Prelude hiding (id, (.))

newtype Op cat a b = Op {runOp :: cat b a}

-- | An arrow in C^op, the opposite category of C, where objects are of type 'obj'
type OpArrow obj u v = Op (Arrow obj) u v

instance (Category cat) => Category (Op cat) where
  id = Op id
  Op g . Op f = Op (f . g)

-- | An arrow in a category where objects are of type 'obj'
data Arrow obj (u :: obj) (v :: obj) where
  -- The identity arrow for an object 'u' in a category
  Id :: Arrow obj u u
  -- A named arrow in a category, representing a morphism from object 'u' to object 'v'
  Path :: String -> Arrow obj u v
  -- Composition of two arrows: if f: u -> v and g: v -> w, then g . f: u -> w
  Comp :: Arrow obj v w -> Arrow obj u v -> Arrow obj u w

instance Category (Arrow obj) where
  -- Identity arrows
  id = Id
  Id . f = f
  g . Id = g
  -- Composition of arrows
  g . f = Comp g f

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

-- | A presheaf over a category 'cat' with values of type 's' is a contravariant functor from
-- 'cat' to 's' (the category of sets, or set-like structures)
data Presheaf cat s where
  Presheaf
    :: { restrict :: forall u v. Op cat u v -> s v -> s u
       }
    -> Presheaf cat s

-- Recursive restrict helper function for presheaves
restrict' :: OpArrow obj u v -> Section val v -> Section val u
restrict' (Op Id) secV = secV
restrict' (Op (Comp g f)) secV =
  let secMid = restrict' (Op f) secV -- pull secV back to the intermediate object v1
   in restrict' (Op g) secMid -- pull secMid back to u
restrict' (Op (Path name)) secV = Restrict (Path name) secV

-- | Pullback a section of a presheaf 'p' along an arrow in the category.
-- Given a presheaf 'p', an arrow 'arrow' from 'v' to 'u', and a section 'sec' over 'v', this function
-- returns a section over 'u'.
pullback :: Presheaf cat s -> cat v u -> s v -> s u
pullback p arrow sec = restrict p (Op arrow) sec

-- | Construct a presheaf of sections over a category of arrows and a given value type for the sections.
mkPresheaf :: Presheaf (Arrow obj) (Section val)
mkPresheaf = Presheaf{restrict = restrict'}