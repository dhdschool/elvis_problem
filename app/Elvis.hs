
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

module Elvis where

import FixedVector
import Data.Singletons
import Data.Number.BigFloat

type R = BigFloat Prec10

precision :: Integer
precision = 8

-- A zero vector of size n
zeroVecs_ :: Sing n -> Vec n R
zeroVecs_ = \case
    SZ -> Nil
    SS l -> 0:# zeroVecs_ l

-- A zero vector that has an implicit size of n using haskell's powerfull type system
-- For example, if you had (3:#2:#Nil) + zeroVecs, you would not have to define the size of
-- zeroVecs because addition is only defined between vectors of the same size,
-- so Haskell assumes that the size of the zeroVec is 2

zeroVecs :: (SingI n) => Vec n R
zeroVecs = zeroVecs_ sing

--Identity matrix of size n x n
baseVecs_ :: Sing n -> Vec n (Vec n R)
baseVecs_ = \case
    SZ -> Nil
    SS l -> (1 :# zeroVecs_ l) :# ((0 :#) <$> baseVecs_ l)

--Identity square matrix of implicit size
baseVecs :: (SingI n) => Vec n (Vec n R)
baseVecs = baseVecs_ sing

-- Negative identity matrix of size n x n
negativeVecs_ :: Sing n -> Vec n (Vec n R)
negativeVecs_ = \case
    SZ -> Nil
    SS l -> (-1 :# zeroVecs_ l) :# ((0 :#) <$> negativeVecs_ l)

--Negative identity square matrix of implicit size
negativeVecs :: (SingI n) => Vec n (Vec n R)
negativeVecs = negativeVecs_ sing

-- Associated type signatures and operations with real vectors
class RealVec v where  
    norm :: v -> R 
    (<.>) :: v -> v -> R
    (|+|) :: v -> v -> v
    (|-|) :: v -> v -> v
    (|*|) :: R -> v -> v
    unit :: v -> v
    bisector :: v -> v -> v
    getAngle :: v -> v -> R
    dirDerivative :: (v -> R) -> v -> v -> R
    grad :: (v -> R) -> v -> v
    integral :: (v -> R) -> v -> R

-- Norm of vector of size n
norm_ :: Sing n -> Vec n R -> R
norm_ = \case
        SZ -> \_ -> 0
        SS l -> \(x:#xs) -> x^(2::Integer) + norm_ l xs

-- Dot product of vector of size n
dot_ :: Sing n -> Vec n R -> Vec n R -> R
dot_ = \case
    SZ -> \_ -> \_ -> 0
    SS l -> \(x:#xs) -> \(y:#ys) -> (x*y) + dot_ l xs ys

-- Addition of two vectors of size n
add_ :: Sing n -> Vec n R -> Vec n R -> Vec n R
add_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \(x:#xs) -> \(y:#ys) -> (x+y) :# add_ l xs ys

-- Subtraction of two vectors of size n
sub_ :: Sing n -> Vec n R -> Vec n R -> Vec n R
sub_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \(x:#xs) -> \(y:#ys) -> (x-y) :# sub_ l xs ys

-- Scalar multiplication of two vectors of size n
scmult_ :: Sing n -> R -> Vec n R -> Vec n R
scmult_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \r -> \(x:#xs) -> (r*x) :# scmult_ l r xs

-- Declares that all Vectors of size n that have real values are real vectors
instance (SingI (n::Nat)) => RealVec (Vec n R) where  
    -- Implicit norm
    norm v = sqrt $ norm_ sing v
    -- Implicit infix operator for dot product
    (<.>) x y = dot_ sing x y
    -- Implicit infix operator for vector addition
    (|+|) x y = add_ sing x y
    -- Implicit infix operator for vector subtraction
    (|-|) x y = sub_ sing x y
    -- Implicit infix operator for scalar multiplication
    (|*|) r x = scmult_ sing r x
    -- The below operations are automatically implicit due to the implicit nature of the functions they use

    -- Returns the unit vector
    unit x = (1/(norm x)) |*| x
    -- Returns the a bisecting vector of two vectors
    bisector x y = ((norm y) |*| x) |+| ((norm x) |*| y)
    -- Returns the angle between two vectors
    getAngle x y = acos ((x<.>y) / ( (norm x) * (norm y)) )
    -- Returns the directional derivative of f of x in direction v (using numerical differentiation)
    dirDerivative f x v = (f(x |+| (h|*|v)) - f(x)) / h  where
         h = (10::R) ** (- (fromInteger precision))
    -- Returns the gradient of f at x (assumed to exist due to convexity of problems)
    grad f x = generate (\i -> dirDerivative f x (index i baseVecs))
    -- Returns the line integral of a vector valued function (using Riemann sums)
    integral f x = (sum $ f <$> (area)) / (fromInteger h) where
        h = precision ^ 2
        area = pairs (|-|) ((|*|x) <$> bounds)
        bounds = ((fromInteger) <$> [0..h])

-- Auxillery helper function, applys binary operator adjacent elements of a list and returns a list of size n-1,
-- of of size 0 if there are not enough elements in the list

pairs :: (a -> a -> a) -> [a] -> [a]
pairs f (x:(x2:xs)) = [f x2 x] ++ pairs f (x2:xs)
pairs _ _ = []

-- ConFun stands for Constructor Function, this is a list of minkowski additions that comprise a single set
type ConFun n = [(Vec n R -> R)]

--VSet stands for Vector Set, has m constraints that are each comprised of some Constructor Function
type VSet m n = Vec m (ConFun n)

--CSet stands for Convex sets, this typeclass defines operations on convex sets
class CSet m n where
    contains :: Vec n R -> VSet m n -> Bool
    intersection :: (CSet k n) => VSet k n -> VSet m n -> VSet (k+m) n
    add :: (CSet k n) => VSet m n -> VSet k n -> VSet (m*k) n
    proj :: VSet m n  -> Vec n R -> Vec n R
    distance :: Vec n R -> VSet m n -> R

--implicit function for resolving minkowski sums
contains_add :: [R] -> R
contains_add [] = 0
contains_add (x:xs) = min x (contains_add xs)

--function for resolving intersections given the size of the vector and the number of constraints
contains_inter :: (Sing n, Sing m) -> Vec n R -> VSet m n -> Bool
contains_inter = \case
    (SZ, _)  -> \_ -> \_ -> True
    (_, SZ) -> \_ -> \_ -> False  
    (l1, SS l2) -> \v -> \(f:#fs) -> ((contains_add $ f <*> (pure v)) <=0) && contains_inter (l1, l2) v fs


instance (SingI m, SingI n) => CSet m n where
    -- Implicit function to determine if a vector is contained in a set
    contains v f = contains_inter (sing, sing) v f
    -- Function that returns a new set from the intersection of two sets
    intersection f1 f2 = (f1 |++| f2)
    --distance 
    --add f1 f2 = f1 <*> ((++) <$> f2)

-- The function that defines a ball in R n of radius r
ballf :: (RealVec (Vec n R)) => R -> Vec n R -> R
ballf r x = norm x - r

-- The set formed by the ball function
ball :: (SingI n, RealVec (Vec n R)) => R -> VSet (Lit 1) n
ball r = [ballf r]:#Nil

