
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

import Data.Singletons ( SingI(..) )
import Data.Foldable (maximumBy)

--VSet stands for Vector Set, has m constraints
--type VSet m n = Vec m (Vec n R -> R)
type MinkowskiSum n = [Vec n R -> R]
type VSet n = [(MinkowskiSum n)]


--CSet stands for Convex sets, this typeclass defines operations on convex sets
class (RealVec (Vec n R)) => CSet n where
    contains :: Vec n R -> VSet n -> Bool
    (<:>) ::  VSet n -> VSet n -> VSet n
    (<+>) :: (CSet n) => VSet n -> MinkowskiSum n -> VSet n
    proj :: VSet n  -> Vec n R -> Vec n R
    distance :: VSet n -> Vec n R -> R
    

--implicit function for resolving vector existance in minkowski sums
contains_add :: (RealVec (Vec n R), SingI n) => MinkowskiSum n -> Vec n R -> Bool
contains_add gs v = norm v <= sum (liftA2 single_dist gs (pure v))

--function for resolving vector existance in set intersections given the size of the vector and the number of constraints
contains_inter :: (SingI n) => Vec n R -> VSet n -> Bool
contains_inter _ [] = True
contains_inter v (f:fs) = ((contains_add f v)) && contains_inter v fs
    

-- function to determine which vector in a list of vectors has the highest norm
-- maxVec :: (SingI n) => (Vec n R -> R) -> Vec n R -> [Vec n R] -> Vec n R
-- maxVec _ _ [] = zeroVecs
-- maxVec compare_f x (v:vs)
--     | compare_f (x |-| v) >= compare_f (maxVec compare_f x (vs)) = v
--     | otherwise = maxVec compare_f x vs


instance (SingI n, RealVec (Vec n R)) => CSet n where
    -- Implicit function to determine if a vector is contained in a set
    contains v f = contains_inter v f
    -- Function that returns a new set from the intersection of two sets
    (<:>) g f = g ++ f
    -- Function that returns a new set from the minkowski sum of a set and some minkowski sum (which may be with zero) 
    -- this currently does not support the minkowski sum between two intersections comprised of minkowski sums
    -- only the minkowski sum of a single minkowski sum and some intersection of minkowski sums
  
    (<+>) g f = pure (++f) <*> g
    -- Projection of a vector onto the set
    proj g v
        | (contains v g) = v
        | otherwise = y where
            y = maximumBy (\i -> \j -> compare (norm $ v|-|i) (norm $ v|-|j)) gs 
            gs = (minkowskiProjections <$> g)
            minkowskiProjections f = foldr (|+|) zeroVecs ((single_proj <$> f) <*> pure v)
    
    -- Distance of a vector from the closest point on the set
    distance g v = norm (v |-| (proj g v))



ellipsoid :: (VSet (Lit 2))
ellipsoid = [ellipsoids]

ellipsoidf :: Vec (Lit 2) R -> R
ellipsoidf v = ((index FZ v)^(2::Integer) + (((index (FS FZ) v) + 1)^(2::Integer))/4 - 1)

ellipsoids :: MinkowskiSum (Lit 2)
ellipsoids = [ellipsoidf]