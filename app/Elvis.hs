
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
import RealVector
import Proj

import Data.Singletons ( Sing, SingI(..) )

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
contains_add :: [Vec n R -> R] -> Vec n R -> Bool
contains_add gs v = norm v <= sum (liftA2 single_dist gs (pure v))

--function for resolving intersections given the size of the vector and the number of constraints
contains_inter :: (Sing n, Sing m) -> Vec n R -> VSet m n -> Bool
contains_inter = \case
    (SZ, _)  -> \_ -> \_ -> True
    (_, SZ) -> \_ -> \_ -> False  
    (l1, SS l2) -> \v -> \(f:#fs) -> ((contains_add f v)) && contains_inter (l1, l2) v fs


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

