
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
{-# LANGUAGE DeriveGeneric #-}

module Region where

import FixedVector
import RealVector
import ConvexSet
import Data.Hashable
import Data.Singletons
import Data.List (permutations)
import Data.Kind (Type)
import qualified Data.HashMap.Strict as HashMap


type VelocityRegions n = HashMap.HashMap (Region n) (VSet n)

-- Halfspace defined by normal vector and dot product threshold r
data HalfSpace :: Nat -> Type where
    Zeta :: (RealVec (Vec n R)) => Vec n R -> R -> HalfSpace n

instance Eq (HalfSpace n) where
    (==) (Zeta n1 r1) (Zeta n2 r2) 
        | (unit n1 == unit n2) && (norm n1 * r1 == norm n2 * r2) = True
        | otherwise = False

-- Suboptimal built in-hash, should be replaced for performance
instance (SingI n) => Hashable (HalfSpace n) where
    hashWithSalt :: Int -> HalfSpace n -> Int
    hashWithSalt salt (Zeta n r) = round (norm (n |+| vecreplicate (fromIntegral salt)) + r)


-- Region formed by the intersection of a list of halfspaces
type Region n = [HalfSpace n]

-- Determining whether a vector is within a halfspace
in_space :: (RealVec (Vec n R)) => HalfSpace n -> Vec n R -> Bool
in_space (Zeta n r) v = (n <.> v) <= r


-- Converting a halfspace into a constraint function (these will not be bounded by definition, use with caution)
from_halfspace :: (RealVec (Vec n R)) => HalfSpace n -> Vec n R -> R
from_halfspace (Zeta n r) v = (n <.> v) - r


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
get_dual (Zeta n r) = Zeta (zeroVecs |-| n) (-r)

get_interface :: HalfSpace n -> Vec n R
get_interface (Zeta n r) = (r * (pnorm 1 n_normal)) |*| n_normal where
    n_normal = unit n

-- Returns the counterpart of a given halfspace and a vector thats guaranteed to be on the intersection between halfspaces
get_dual_interface :: (SingI n) => HalfSpace n -> (HalfSpace n, Vec n R)
get_dual_interface (Zeta n r) = (Zeta (zeroVecs |-| n) (-r), (get_interface (Zeta n r)))


-- Generates regions from a given set of halfspaces (without duals) that creates regions spanning R^n
-- for example, if you had two halfspaces this would return you the four regions created by their intersections
generate_Rn :: (SingI n) => [HalfSpace n] -> [Region n]
generate_Rn hs = (hs ++ (get_dual <$> hs))

-- Gets the interfaces that border a given region and a vector on that interface
get_boundaries :: (SingI n) => Region n -> [Region n]
get_boundaries [] = []
get_boundaries (h:hs) = (([h, (get_dual h)] ++ hs) : ((h:) <$> (get_boundaries hs)))

-- Gets the regions that border a given region
get_adjacent_region :: (SingI n, RealVec (Vec n R)) => Region n -> [(Region n, Vec n R)]
get_adjacent_region [] = []
get_adjacent_region (h:hs) = zip (([adj_region] ++ hs) : ((h:) <$> (tail_region))) (adj_vec:tail_vec) where
    (adj_region, adj_vec) = get_dual_interface h
    (tail_region, tail_vec) = unzip $ get_adjacent_region hs

    
-- There's definitely a faster way to implement this than search (from O(n) -> O(1))
-- This searches all regions in a given list and returns the region that contains the points, with an empty
-- list if no regions contain the point

region_from_points_ :: (RealVec (Vec n R), SingI n) => [Region n] -> Vec n R -> Region n
region_from_points_ (r:rs) x
    | in_region r x = r  
    | otherwise = region_from_points_ rs x
region_from_points_ _ _ = []

region_from_points :: (RealVec (Vec n R), SingI n) => VelocityRegions n -> Vec n R -> Region n 
region_from_points rmap x = region_from_points_ (HashMap.keys rmap) x