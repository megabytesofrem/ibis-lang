{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Defines a Grothendieck site, which is a category equipped with a topology that specifies
-- which families of morphisms (sieves) are considered to be covers of objects in the category.
module Category.Grothendieck where

-- Implementation based on the definitions from the following sources:
-- https://ncatlab.org/nlab/show/Grothendieck+topology
-- https://ncatlab.org/nlab/show/cover
-- "Sheaf Theory Through Examples (Abridged Version)" by Daniel Roslak

import Control.Category
import Prelude hiding (id, (.))

-- Arrow types in the Grothendieck topology are arrows between sections of a presheaf
-- representing inclusion, composition and identity between objects in a category.
import Category.Presheaf.Arrow (Arrow)

-- | A sieve on an object 'c' is a family of arrows into 'c' that is closed under precomposition
-- (leftmost composition) with any arrow in the category, representing a collection of subobjects or
-- covers of 'c'
data Sieve obj (c :: obj) where
  Sieve :: {contains :: forall (d :: obj). Arrow obj d c -> Bool} -> Sieve obj c

-- | A Grothendieck site topology 'J' assigns to each object 'c' in the category a collection of
-- covering sieves, satisfying the following axioms:
--
-- 1. Stability under base change: If F is a sieve covering 'c' and g: d -> c is any morphism,
-- then the pullback sieve g^*F covers 'd'
--
-- 2. Local character: Let F, F' be sieves on c. Suppose F covers c and the pullback sieve g^*F' covers d
-- for every arrow g: d -> c in F. Then F' covers c.
--
-- 3. The maximal sieve: The maximal sieve @hom(-, c) -> hom(-, c)@ is always a covering sieve.
data SiteTopology obj where
  SiteTopology
    :: { isCover :: forall (c :: obj). Sieve obj c -> Bool
       }
    -> SiteTopology obj

-- | A category equipped with a Grothendieck topology, forming a Grothendieck site: C(J),
-- where C is a category and J is a Grothendieck topology.
data GrothendieckSite obj where
  GrothendieckSite
    :: { topology :: SiteTopology obj -- The Grothendieck topology J on the category C
       }
    -> GrothendieckSite obj

-- | Pullback a sieve @F@ along a morphism @g: d -> c@ to obtain a sieve on @d@.
pullbackSieve :: Arrow obj d c -> Sieve obj c -> Sieve obj d
pullbackSieve g (Sieve f) = Sieve $ \h -> f (g . h)

-- | Check if a sieve @F@ is a covering sieve in the Grothendieck site @C(J)@.
isCoveringSieve :: GrothendieckSite obj -> Sieve obj c -> Bool
isCoveringSieve (GrothendieckSite j) sieve = isCover j sieve

-- | Construct a Grothendieck site from a given covering sieve predicate.
--
-- The predicate must satisfy the Grothendieck topology axioms.
mkSite :: (forall (c :: obj). Sieve obj c -> Bool) -> Maybe (GrothendieckSite obj)
mkSite isCover' =
  let j = SiteTopology isCover'
      maximalSieve = Sieve $ \_ -> True
   in -- Check if the maximal sieve is a covering sieve in the Grothendieck site
      if isCover' maximalSieve
        then Just (GrothendieckSite j)
        else Nothing