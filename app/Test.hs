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

module Test where
import Region
import Graph 
import Elvis
import FixedVector
import RealVector
import ConvexSet
import RealMatrix
import Data.Maybe (fromJust)
import Unsafe.Coerce

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
test_g41 v = ((index (FZ) v - 1)^(2::Integer))/4 + (((index (FS FZ) v))^(2::Integer)) - 1

test_g42 :: (Vec (Lit 2) R) -> R
test_g42 v = ((index (FZ) v + 1)^(2::Integer))/4 + (((index (FS FZ) v))^(2::Integer)) - 1

test_G4 :: VSet (Lit 2)
test_G4 = [[test_g41], [test_g42]]

test_velocities :: [VSet (Lit 2)]
test_velocities = [test_G1, test_G2, test_G3, test_G4]

test_veldata :: VelocityRegions (Lit 2)
test_veldata = fromJust $ init_velocities test_quadrants test_velocities

test_graph :: Graph (Lit 2)
test_graph = construct_graph test_veldata test_x0 test_x1

test_paths :: [ElvisData (Lit 2)]
test_paths = fromJust $ get_all_paths test_veldata (region_from_points test_veldata test_x0) (region_from_points test_veldata test_x1) test_graph

test_out :: ([Vec (Lit 2) R], R)
test_out = elvis_gen test_x0 test_x1 [test_paths_head] where
    (test_paths_head:test_tail) = test_paths



