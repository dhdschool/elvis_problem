
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

module ConvexSet where


import FixedVector
import RealVector
import Proj

import Data.Singletons ( Sing, SingI(..) )

--VSet stands for Vector Set, has m constraints
--type VSet m n = Vec m (Vec n R -> R)
type MinkowskiSum n = [Vec n R -> R]
type VSet m n = Vec m (MinkowskiSum n)

--CSet stands for Convex sets, this typeclass defines operations on convex sets
class (RealVec (Vec n R), Applicative (Vec m)) => CSet m n where
    contains :: Vec n R -> VSet m n -> Bool
    (<:>) :: (CSet m n, CSet k n) => VSet m n -> VSet k n -> VSet (m+k) n
    (<+>) :: (CSet m n) => VSet m n -> MinkowskiSum n -> VSet m n
    proj :: VSet m n  -> Vec n R -> Vec n R
    distance :: VSet m n -> Vec n R -> R
    

--implicit function for resolving vector existance in minkowski sums
contains_add :: (RealVec (Vec n R), SingI n) => MinkowskiSum n -> Vec n R -> Bool
contains_add gs v = norm v <= sum (liftA2 single_dist gs (pure v))

--function for resolving vector existance in set intersections given the size of the vector and the number of constraints
contains_inter :: (SingI n) => (Sing m) -> Vec n R -> VSet m n -> Bool
contains_inter = \case
    (SZ) -> \_ -> \_ -> True  
    (SS l2) -> \v -> \(f:#fs) -> ((contains_add f v)) && contains_inter (l2) v fs

-- function to determine which vector in a list of vectors has the highest norm
maxVec :: (SingI n) => (Vec n R -> R) -> Vec n R -> Vec m (Vec n R) -> Vec n R
maxVec _ _ Nil = zeroVecs
maxVec compare_f x (v:#vs)
    | norm (x |-| v) >= norm (maxVec compare_f x (vs)) = v
    | otherwise = maxVec compare_f x vs


instance (SingI m, SingI n, RealVec (Vec n R), Applicative (Vec m)) => CSet m n where
    -- Implicit function to determine if a vector is contained in a set
    contains v f = contains_inter (sing) v f
    -- Function that returns a new set from the intersection of two sets
    (<:>) g f = g |++| f
    -- Function that returns a new set from the minkowski sum of a set and some minkowski sum (which may be with zero) 
    -- this currently does not support the minkowski sum between two intersections comprised of minkowski sums
    -- only the minkowski sum of a single minkowski sum and some intersection of minkowski sums
  
    (<+>) g f = pure (++f) <*> g
    -- Projection of a vector onto the set
    proj g v
        | (contains v g) = v
        | otherwise = y where
            y = maxVec (norm) v gs
            gs :: Vec m (Vec n R)
            gs = (minkowskiProjections <$> g)
            minkowskiProjections f = foldr (|+|) zeroVecs ((single_proj <$> f) <*> pure v)
    
    -- Distance of a vector from the closest point on the set
    distance g v = norm (v |-| (proj g v))

-- The set formed by the ball function
ball :: (SingI n, RealVec (Vec n R)) => R -> VSet (Lit 1) n
ball r = [ballf r]:#Nil

unit_circle :: (VSet (Lit 1) (Lit 2))
unit_circle = ball 1

ellipsoid :: (VSet (Lit 1) (Lit 2))
ellipsoid = ellipsoids:#Nil

ellipsoidf :: Vec (Lit 2) R -> R
ellipsoidf v = ((index FZ v)^(2::Integer) + (((index (FS FZ) v) + 1)^(2::Integer))/4 - 1)

ellipsoids :: MinkowskiSum (Lit 2)
ellipsoids = [ellipsoidf]