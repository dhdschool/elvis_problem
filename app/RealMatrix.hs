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

module RealMatrix where

import FixedVector
import RealVector
import Proj (precision_)
import Data.Singletons

-- Zero matrix of given size
zeroMat_ :: (SingI n) => Sing m -> Matrix m n
zeroMat_ = \case
    SZ -> Nil
    SS l -> zeroVecs :# zeroMat_ l

-- Zero matrix of implicit size
zeroMat :: (SingI m, SingI n) => Matrix m n
zeroMat = vecreplicate (vecreplicate (0::R))

-- This one sits on a pallid bust of Pallis above my chamber door
-- The identity tensor of R ^ (n x m x n x m) represented here as a hypercube
-- This provides base matricies for derivatives in the direction of matricies given a specified
-- number of constraints m that are vectors in R^n
identityTensor_ :: (RealVec (Vec n R), SingI n) => Sing m -> Vec m (Vec n (Matrix m n))
identityTensor_ = \case
    SZ -> Nil 
    SS l -> thisLayer :# nextLayer where
        thisLayer = generate (\i -> (index i baseVecs) :# zeroMat_ l)
        nextLayer = zeroApply (identityTensor_ l)
        zeroLayer m = (zeroVecs:#) <$> m
        zeroApply m = (zeroLayer) <$> m

-- Implicitly sized identity tensor
identityTensor :: (RealVec (Vec n R), SingI n, SingI m) => Vec m (Vec n (Matrix m n))
identityTensor = identityTensor_ sing

-- Wrapper type for matrices
type Matrix m n = Vec m (Vec n R)

-- Class defining operations on matrices
class RealMat m where
    (#+#) :: m -> m -> m
    (#-#) :: m -> m -> m
    scal :: R -> m -> m
    mat_dirDerivative :: (m -> R) -> m -> m -> R
    mat_grad :: (m -> R) -> m -> m


mat_add_ :: (RealVec (Vec n R)) => Sing m -> Matrix m n -> Matrix m n -> Matrix m n
mat_add_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \(x:#xs) -> \(y:#ys) -> (x|+|y) :# (mat_add_ l xs ys) 

mat_sub_ :: (RealVec (Vec n R)) => Sing m -> Matrix m n -> Matrix m n -> Matrix m n
mat_sub_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \(x:#xs) -> \(y:#ys) -> (x|-|y) :# (mat_sub_ l xs ys) 

mat_scalar_mult_ :: (RealVec (Vec n R)) => Sing m -> R -> Matrix m n -> Matrix m n
mat_scalar_mult_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \t -> \(x:#xs) -> (t|*|x) :# (mat_scalar_mult_ l t xs)

instance (SingI m, RealVec (Vec n R), SingI n) => RealMat (Matrix m n) where
    (#+#) a b = mat_add_ sing a b
    (#-#) a b = mat_sub_ sing a b
    scal t a = mat_scalar_mult_ sing t a
    mat_dirDerivative f x m = (f (x #+# (scal t m)) - f x) / t where
        t = 10 ** (- (fromInteger precision))
    mat_grad f a = generate (\i -> 
        generate(\j -> mat_dirDerivative f a (index j (index i identityTensor)) )) 

-- Gradient descent across a matrix valued function
-- b is the precision cutoff
-- cost is the matrix valued function 
-- grad cost is the matrix gradient of the cost function
-- ys0 is the starting matrix
-- episilon is the adjusting level of precision within the minimum

matrix_gradient_descent_ :: (RealVec (Vec n R), RealMat (Matrix m n)) => R -> (Matrix m n -> R) -> (Matrix m n -> Matrix m n) -> Matrix m n -> R -> Matrix m n
matrix_gradient_descent_ b cost grad_cost ys0 episilon
    | b >= precision_ = ys0 
    | cost (ys) > cost (ys0) = matrix_gradient_descent_ (b+1) cost grad_cost ys0 (episilon/2)
    | cost (ys0) >= cost (ys) = matrix_gradient_descent_ (b+1) cost grad_cost ys (episilon*2) 
    | otherwise = ys0 where
        ys = ys0 #-# (scal episilon (grad_cost ys0))

matrix_gradient_descent :: (RealVec (Vec n R), RealMat (Matrix m n)) => (Matrix m n -> R) -> Matrix m n -> Matrix m n
matrix_gradient_descent cost ys0 = matrix_gradient_descent_ 0 cost (mat_grad cost) ys0 1