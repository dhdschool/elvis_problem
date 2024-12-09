
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures#-}
{-# LANGUAGE EmptyCase#-}
{-# LANGUAGE ExistentialQuantification#-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE GADTs#-}
{-# LANGUAGE InstanceSigs#-}
{-# LANGUAGE KindSignatures#-}
{-# LANGUAGE NoCUSKs#-}
{-# LANGUAGE NoNamedWildCards#-}
{-# LANGUAGE NoStarIsType#-}
{-# LANGUAGE PolyKinds#-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE StandaloneDeriving#-}
{-# LANGUAGE StandaloneKindSignatures#-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TypeAbstractions#-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE TypeFamilies#-}
{-# LANGUAGE TypeOperators#-}
{-# LANGUAGE UndecidableInstances#-}
{-# LANGUAGE LambdaCase #-}
{-# Language MultiParamTypeClasses#-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- Do dogs know calculus?
module Elvis where

import FixedVector
import RealVector
import ConvexSet

import Data.Graph
import qualified Data.HashSet

import Data.Kind (Type)
import Data.Singletons
import Proj (precision_)
import Data.List (permutations)

-- Halfspace defined by normal vector and dot product threshold r
data HalfSpace :: Nat -> Type where
    Zeta :: (RealVec (Vec n R)) => Vec n R -> R -> HalfSpace n

-- Region formed by the intersection of a list of halfspaces
type Region n = [HalfSpace n]

-- Determining whether a vector is within a halfspace
in_space :: (RealVec (Vec n R)) => HalfSpace n -> Vec n R -> Bool
in_space (Zeta n r) v = (n <.> v) <= r

-- Cost function associated with moving in a velocity set towards a point
cost_function :: (RealVec (Vec n R), CSet m n) => VSet m n -> Vec n R ->  Vec n R -> R
cost_function g x y = norm (y |-| x) / norm (proj g y)

-- Converting a halfspace into a constraint function (these will not be bounded by definition, use with caution)
from_halfspace :: (RealVec (Vec n R)) => HalfSpace n -> Vec n R -> R
from_halfspace (Zeta n r) v = (n <.> v) - r

--Because this function is convex, gradient descent is guaranteed to converge (and we can do so rather fast using
-- exponential/binary search)

-- Upon writing this I realize that my previous approximation methods are contrived versions of gradient descent

gradient_descent_ :: (RealVec (Vec n R)) => R -> (Vec n R -> Vec n R) -> Vec n R -> R -> Vec n R
gradient_descent_ b gradient y0 episilon
    | b >= precision_ = y0
    | norm (gradient descend) > norm (gradient y0) = gradient_descent_ (b+1) gradient y0 (episilon/2)
    | norm (gradient y0) >= norm (gradient descend) = gradient_descent_ (b+1) gradient descend (episilon*2) 
    | otherwise = y0 where
        descend = y0 |-| (episilon |*| gradient(y0))

-- Solves the elvis problem with a single interface and two constraint sets, where x0 is on the side of the interface
-- associated with g0, and x1 is on the side associated with g1
elvis_single :: (RealVec (Vec n R), CSet m n, CSet k n) => VSet m n ->  Vec n R -> VSet k n -> Vec n R -> HalfSpace n -> Vec n R
elvis_single g0 x0 g1 x1 h= ((gradient_descent_ 0) $! (grad cost)) y 1 where
    y = interface_intersect x0 x1 h
    cost v = (cost_function g0 x0 v) + (cost_function g1 x1 v)

-- Gives the vector on an interface assuming that the interface is between the vectors x0 and x1
interface_intersect :: (RealVec (Vec n R)) => Vec n R -> Vec n R -> HalfSpace n -> Vec n R
interface_intersect x0 x1 (Zeta n r) = lambda |*| (x1|-|x0) where
    lambda = r / (n<.>x1 - n<.>x0)

-- Returns whether or not a given vector is contained within a region formed by the intersection of halfspaces
in_region :: (SingI n, RealVec (Vec n R)) => Region n -> Vec n R -> Bool
in_region region v = foldr (&&) True ((in_space <$> region) <*> pure v)

-- Gives a list containing the given halfspace and the dual halfspace
whole_space :: (SingI n) => HalfSpace n -> [HalfSpace n]
whole_space h = [h, get_dual h]

-- Returns the counterpart of a given halfspace
get_dual :: (SingI n) => HalfSpace n -> HalfSpace n
get_dual (Zeta n r) = Zeta (zeroVecs |-| n) r

-- Returns the regions in space created by the intersection of a list of halfspaces
get_regions :: [HalfSpace n] -> [Region n]
get_regions hs = permutations hs

-- Gets the interfaces that border a given region
get_boundaries :: (SingI n) => Region n -> [Region n]
get_boundaries [] = []
get_boundaries (h:hs) = (([h, (get_dual h)] ++ hs) : ((h:) <$> (get_boundaries hs)))

-- Gets the regions that border a given region
get_adjacent_region :: (SingI n) => Region n -> [Region n]
get_adjacent_region [] = []
get_adjacent_region (h:hs) = (([(get_dual h)] ++ hs) : ((h:) <$> (get_regions hs)))