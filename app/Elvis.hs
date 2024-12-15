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
import Data.Foldable (Foldable (toList))

type ElvisData n = [(Vec n R, VSet n, Region n)]

error_punish :: R
error_punish = 10

-- Cost function associated with moving in a velocity set towards a point (vector valued function of two inputs)
cost_function_single :: (RealVec (Vec n R), CSet n) => Region n -> VSet n -> Vec n R ->  Vec n R -> R
cost_function_single sigma g start end = (norm (end |-| start) / norm (proj g (end |-| start))) + (indicator_function error_punish sigma end) 

-- Generalized cost function of the cost function above (matrix valued function)
cost_function :: (RealVec (Vec n R), RealMat (Matrix (S m) n), CSet n, SingI m, Applicative (Vec m)) =>
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

-- Rather than infinity, this function penalizes a vector that is not on an interface by some constant c
indicator_function :: (RealVec (Vec n R)) => R -> Region n -> Vec n R -> R
indicator_function cost region x = cost * min_dist where
    min_dist = minimum (abs <$> (( (from_halfspace) <$> region) <*> pure x))


--Because this function is convex, gradient descent is guaranteed to converge (and we can do so rather fast using
-- exponential/binary search)

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

--Solves the elvis problem with a list of interfaces to cross over and their corresponding velocity sets
elvis_single_multiple_interface :: (RealVec (Vec n R), CSet n, SingI n) =>
    Vec n R -> Vec n R -> ElvisData n -> ([Vec n R], R, Sing (S m))

elvis_single_multiple_interface x0 x1 data_list = (toList mf, cost mf, matrix_size) where
    ((m0, g, r), SS matrix_size) = to_matrix_and_constraints data_list
    mf = withSingI matrix_size (matrix_gradient_descent cost m0)

    size_minus_one :: (Sing (S n) -> Sing n)
    size_minus_one = \case
        SS l -> l

    cost = withSingI (size_minus_one matrix_size) (cost_function x0 x1 r g)

-- Solves the generalized elvis problem given a list of paths and their associated velocity sets
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


-- Internal function for converting a list into a matrix for use in matrix gradient descent
-- This required some (in my opinion) tricky usage of dependent types 

to_matrix_and_constraints :: (SingI n) => ElvisData n -> ((Matrix m n, Vec (S m) (VSet n), Vec (S m) (Region n)), Sing (S m))
to_matrix_and_constraints lst = ((interfaces, sets, regions), size)
    where
        (val, size) = fromListSafe lst
        (vecs, sets, regions) = unzipVec3 val
        interfaces = reverseTail_ size vecs
  
