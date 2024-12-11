
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

-- Do dogs know calculus?
module Elvis where

import FixedVector
import RealVector
import ConvexSet
import Region
import RealMatrix

import qualified Data.HashSet as HashSet
import Data.Singletons
import Proj (precision_)
import Data.Foldable

newtype VelocityRegion n = M (Region n, VSet n)

-- Cost function associated with moving in a velocity set towards a point (vector valued function of two inputs)
cost_function_single :: (RealVec (Vec n R), CSet n) => VSet n -> Vec n R ->  Vec n R -> R
cost_function_single g x y = norm (y |-| x) / norm (proj g (y |-| x))

-- Generalized cost function of the cost function above (matrix valued function)
-- This function assumes that the VSet list is the same size as your matrix without checking (for silly reasons)
-- If this is not ensured, you will recieve errors as this function implements partial (unsafe) functions
cost_function :: (RealVec (Vec n R), RealMat (Matrix (S m) n), CSet n) =>  Vec n R -> Vec n R -> [VSet n] -> Matrix (S m) n -> R
cost_function x0 x1 g m = cost_function_single g0 x0 y0 + mat_cost + cost_function_single gf x1 yf where
    y0 = getFirst m
    yf = getLast m
    g0 = head g
    gf = last g
    gs = reverseTail (tail g)
    lst = toList m
    differences = pairs (|-|) lst
    velocity = norm <$> ((proj <$> gs) <*> differences)
    dist = norm <$> differences
    mat_cost = sum $ liftA2 (/) dist velocity


reverseTail :: [a] -> [a]
reverseTail (_:[]) = []
reverseTail (v:vs) = [v] ++ (reverseTail vs)

--Because this function is convex, gradient descent is guaranteed to converge (and we can do so rather fast using
-- exponential/binary search)

-- Upon writing this I realize that my previous approximation methods are contrived versions of gradient descent

gradient_descent_ :: (RealVec (Vec n R)) => R -> (Vec n R -> R) -> (Vec n R -> Vec n R) -> Vec n R -> R -> Vec n R
gradient_descent_ b cost gradient y0 episilon
    | b >= precision_ = y0
    | cost descend > cost y0 = gradient_descent_ (b+1) cost gradient y0 (episilon/2)
    | cost y0 >= cost descend = gradient_descent_ (b+1) cost gradient descend (episilon*2) 
    | otherwise = y0 where
        descend = y0 |-| (episilon |*| gradient(y0))

gradient_descent :: (RealVec (Vec n R)) => (Vec n R -> R) -> Vec n R -> Vec n R
gradient_descent cost y0 = gradient_descent_ 0 cost (grad cost) y0 1

-- Solves the elvis problem with a single interface and two constraint sets, where x0 is on the side of the interface
-- associated with g0, and x1 is on the side associated with g1
elvis_single :: (RealVec (Vec n R), CSet n, CSet n) => VSet n ->  Vec n R -> VSet n -> Vec n R -> HalfSpace n -> Vec n R
elvis_single g0 x0 g1 x1 h= gradient_descent cost y where
    y = interface_intersect x0 x1 h
    cost v = (cost_function_single g0 x0 v) + (cost_function_single g1 v x1)

get_adjacents :: (SingI n, RealVec (Vec n R)) => Region n -> Vec n R -> Vec n R -> [(Region n, Vec n R)]
get_adjacents region x0 x1 = filter (\(_, v) -> norm (v|-|x1) < norm (x0|-|x1)) (get_adjacent_region region)

-- Creates a graph for the use of a graph given a list of regions and a start and end vector,
-- this returns the start region, the end region, and the associated graph

-- I don't think we need [Region n] given now the [Vec n R] but lets not be hasty
elvis_graph :: (RealVec (Vec n R), SingI n) => [Region n] -> Vec n R -> Vec n R ->  (Region n, Region n, [(Region n, [(Region n, Vec n R)])])
elvis_graph regions x0 x1 = (start_region, end_region, graph) where
    (_, graph) = construct_alist_ visited (x0, x1) start_region
    start_region = region_from_points regions x0
    end_region = region_from_points regions x1
    visited = HashSet.empty 

-- Internal adjacency list creator
construct_alist_ :: (SingI n) => HashSet.HashSet (Region n) -> (Vec n R, Vec n R) -> Region n -> (HashSet.HashSet (Region n), [(Region n, [(Region n, Vec n R)])])
construct_alist_ vs (x0, x1) start
    | HashSet.member start vs = (vs, [])
    | otherwise = (vss, [(start, interfaces)] ++ sublist) where
    sublist = foldr (++) [] graphlist
    vss = HashSet.unions (setlist ++ [v])
    v = HashSet.insert start vs
    interfaces = get_adjacents start x0 x1
    (adj, _) = unzip interfaces
    (setlist, graphlist) = unzip ((construct_alist_ v (x0, x1)) <$> adj)

-- You had best ensure that these two are the same size before executing this function
velocity_region_map :: [Region n] -> [VSet n] -> [VelocityRegion n]
velocity_region_map h s = M <$> zip h s 

get_velocity_set :: Region n -> [VelocityRegion n] -> Maybe (VSet n)
get_velocity_set _ [] = Nothing
get_velocity_set region (M (region_i, set_i) : next)
    | region == region_i = Just set_i 
    | otherwise = get_velocity_set region next


dfs_end :: (RealVec (Vec n R), SingI n) => [VelocityRegion n] -> [(Region n, [(Region n, Vec n R)])] -> (Matrix m n, [VSet n])
dfs_end _ [] = (Nil, [])
dfs_end vsregion ((current_region, adj_interfaces) : node_tail) = case maybeVal of 
    Nothing -> []
    Just vset -> (zip vectors (replicate (length vectors) vset)) ++ (dfs_end vsregion node_tail)
    where    
        maybeVal = get_velocity_set current_region vsregion
        (_, vectors) = unzip adj_interfaces
    

test_space_x :: HalfSpace (Lit 2)
test_space_x = Zeta (0:#1:#Nil) 0

test_space_y :: HalfSpace (Lit 2)
test_space_y = Zeta (1:#0:#Nil) 0

test_quadrants :: [Region (Lit 2)]
test_quadrants = generate_Rn [test_space_x, test_space_y]

test_x0 :: Vec (Lit 2) R
test_x0 = (-1):#(1):#Nil

test_x1 :: Vec (Lit 2) R
test_x1 = (1):#(-1):#Nil

--Unit circle
test_g1 :: Vec (Lit 2) R -> R
test_g1 v = (index (dim 1) v) ^ (2::Integer) + (index (dim 2) v) ^ (2::Integer) - 1

test_G1 :: VSet (Lit 2)
test_G1 = [[test_g1]]

--Ellipsoid skewed in the y axis
test_g2 :: Vec (Lit 2) R -> R
test_g2 v = (index (dim 1) v)^(2::Integer) + (((index (dim 1) v) + 1)^(2::Integer))/4 - 1

test_G2 :: VSet (Lit 2)
test_G2 = [[test_g2]]

test_start :: Region (Lit 2)
test_end :: Region (Lit 2)
test_graph :: [(Region (Lit 2), [(Region (Lit 2), Vec (Lit 2) R)])]
(test_start, test_end, test_graph) = elvis_graph test_quadrants test_x0 test_x1

