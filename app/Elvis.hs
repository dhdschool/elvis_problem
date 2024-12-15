
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

import Data.Singletons
import Proj (precision_)
import Graph 
import Data.Foldable (minimumBy, Foldable (toList))

type ElvisData n = [(Vec n R, VSet n, Region n)]

error_punish :: R
error_punish = 10

-- Cost function associated with moving in a velocity set towards a point (vector valued function of two inputs)
cost_function_single :: (RealVec (Vec n R), CSet n) => Region n -> VSet n -> Vec n R ->  Vec n R -> R
cost_function_single sigma g start end = (norm (end |-| start) / norm (proj g (end |-| start))) + (indicator_function error_punish sigma end) 

-- Generalized cost function of the cost function above (matrix valued function)
cost_function :: (RealVec (Vec n R), RealMat (Matrix (S m) n), CSet n, SingI (S m), Applicative (Vec m)) =>
  Vec n R -> Vec n R -> Vec (S (S m)) (Region n) -> Vec (S (S m)) (VSet n) -> Matrix (S m) n -> R
cost_function x0 x1 sigma g m = cost_function_single sigma0 g0 x0 y0 + mat_cost + cost_function_single sigmaf gf yf x1 where
    y0 = getFirst m
    yf = getLast m

    sigma0 = getFirst sigma
    sigmaf = [Zeta x1 ((norm x1) ** 2)]
    sigmas = reverseTail (vecTail sigma)

    g0 = getFirst g
    gf = getLast g
    gs = reverseTail (vecTail g)
    
    differences = vecPairs (|-|) m
    velocity = norm <$> ((proj <$> gs) <*> differences)
    dist = norm <$> differences
    mat_cost = (sum $ liftA2 (/) dist velocity) + errors
    errors = sum (((indicator_function error_punish) <$> sigmas) <*> differences)


indicator_function :: (RealVec (Vec n R)) => R -> Region n -> Vec n R -> R
indicator_function cost region x = cost * min_dist where
    min_dist = minimum (abs <$> (( (from_halfspace) <$> region) <*> pure x))

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
elvis_single :: (RealVec (Vec n R), CSet n) => VSet n ->  Vec n R -> VSet n -> Vec n R -> HalfSpace n -> Vec n R
elvis_single g0 x0 g1 x1 h = gradient_descent cost y where
    y = get_interface h
    cost v = (cost_function_single [h] g0 x0 v) + (cost_function_single [Zeta x1 ((norm x1) ** 2)] g1 v x1)

elvis_single_multiple_interface :: (RealVec (Vec n R), CSet n, SingI n) =>
    Vec n R -> Vec n R -> ElvisData n -> ([Vec n R], R, Sing (S m))

elvis_single_multiple_interface x0 x1 data_list = (toList mf, cost mf, matrix_size) where
    ((m0, g, r), SS matrix_size) = to_matrix_and_constraints data_list
    mf = withSingI matrix_size (matrix_gradient_descent cost m0)

    cost = withSingI matrix_size (cost_function x0 x1 r g)

elvis_gen :: (SingI n) =>
    Vec n R -> Vec n R -> [ElvisData n] -> ([Vec n R], R)


elvis_gen x0 x1 (constraint_data:[]) = (current_solution, current_cost) where
    (current_solution, current_cost, size) = elvis_single_multiple_interface x0 x1 constraint_data

elvis_gen x0 x1 (constraint_data:list_tail)
    | current_cost < last_cost = (current_solution, current_cost)
    | otherwise = (last_solution, last_cost) where
    (last_solution, last_cost) = elvis_gen x0 x1 list_tail
    (current_solution, current_cost, size) = elvis_single_multiple_interface x0 x1 constraint_data
    
elvis_gen _ _ _ = ([], 0)


to_matrix_and_constraints :: (SingI n) => ElvisData n ->  ((Matrix m n, Vec (S m) (VSet n), Vec (S m) (Region n)), Sing (S m))
to_matrix_and_constraints lst = ((interfaces, sets, regions), size)
    where
        (val, size) = fromListSafe lst
        (vecs, sets, regions) = unzipVec3 val
        interfaces = reverseTail_ size vecs
  


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


-- Diamond of side length 1
test_g3 :: Vec (Lit 2) R -> R
test_g3 v = pnorm 1 v - 1

test_G3 :: VSet (Lit 2)
test_G3 = [[test_g3]]

-- intersection of two weird ellipses
test_g41 :: (Vec (Lit 2) R) -> R
test_g41 v = ((index (dim 1) v - 1)^(2::Integer))/4 + (((index (dim 1) v) + 1)^(2::Integer)) - 1

test_g42 :: (Vec (Lit 2) R) -> R
test_g42 v = ((index (dim 1) v + 1)^(2::Integer))/4 + (((index (dim 1) v) + 1)^(2::Integer)) - 1

test_G4 :: VSet (Lit 2)
test_G4 = [[test_g41], [test_g42]]

test_veldata :: Maybe (VelocityRegions (Lit 2))
test_veldata = init_velocities test_quadrants [test_G1, test_G2, test_G3, test_G4]

test_graph :: Maybe (Graph (Lit 2))
test_graph = case test_veldata of 
    Nothing -> Nothing
    Just veldata -> Just $ construct_graph veldata test_x0 test_x1

test_paths :: Maybe [ElvisData (Lit 2)]
test_paths = case (test_graph, test_veldata) of
    (Just graph, Just veldata) -> get_all_paths veldata (region_from_points veldata test_x0) (region_from_points veldata test_x1) graph
    (_, _) -> Nothing 



test_out :: Maybe ([Vec (Lit 2) R], R)
test_out = case test_paths of
    Nothing -> Nothing
    Just constraints -> Just $ elvis_gen test_x0 test_x1 constraints
