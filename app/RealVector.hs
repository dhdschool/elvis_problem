
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


module RealVector where

import FixedVector
import Data.Singletons

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
    pnorm :: Integer -> v -> R

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

pnorm_ :: Sing n -> Integer -> Vec n R -> R
pnorm_ = \case
    SZ -> \_ -> \_ -> 0
    SS l -> \power -> \(x:#xs) -> x^power + pnorm_ l power xs

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
    -- Implicit pnorm for natural p
    pnorm p v = (pnorm_ sing p v)**(1 / fromIntegral p)
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

-- Auxillery helper function, applys binary operator adjacent elements of a list and returns a list of size n-1,
-- of of size 0 if there are not enough elements in the list

pairs :: (a -> a -> a) -> [a] -> [a]
pairs f (x:(x2:xs)) = [f x2 x] ++ pairs f (x2:xs)
pairs _ _ = []

-- The function that defines a ball in R n of radius r
ballf :: (RealVec (Vec n R)) => R -> Vec n R -> R
ballf r x = (norm x)**2 - (r)**2

