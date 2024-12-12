
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

module Graph where

import FixedVector
import RealVector
import ConvexSet
import Region
import Elvis

import qualified Data.HashMap.Strict as HashMap

import Data.Singletons


type Interface n = (Region n, Vec n R)
type Graph n = HashMap.HashMap (Region n) (Node n)
type Node n = [Interface n]

construct_graph_ :: (SingI n) => Graph n -> (Vec n R, Vec n R) -> Region n -> Graph n
construct_graph_ graph (x0, x1) start
    | HashMap.member start graph = graph
    | otherwise = return_graph  where
    return_graph = foldr HashMap.union (HashMap.empty) ((construct_graph_ new_graph (x0, x1)) <$> adjacent_regions)
    new_graph = HashMap.insert start interfaces graph
    interfaces = get_adjacents start x0 x1
    (adjacent_regions, _) = unzip interfaces

-- construct_graph :: (SingI n) => VelocityRegions n -> Vec n R -> Vec n R -> Graph n
-- construct_graph regions x0 x1 = construct_graph_ HashMap.empty (x0, x1) (region_from_points regions x0)

get_velocity_set :: (SingI n) => Region n -> VelocityRegions n -> Maybe (VSet n)
get_velocity_set region rmap = HashMap.lookup region rmap


search_graph_ :: (SingI n) => Region n -> Graph n -> (Maybe (Node n))
search_graph_ search graph = HashMap.lookup search graph


-- dfs :: Region n -> VelocityRegions m n -> [(Region n, (Vec m (Region n, Vec n R)))] -> [(Matrix m n, Vec m (VSet n))]

-- dfs start_region regions ((this_region, (path0_region, path0_vec) :# other_paths) : vectail)
--     | maybe_vset == Nothing = []  
--     where
--     maybe_vset = get_velocity_set this_region

--dfs_wrapper :: [VelocityRegion m n] -> [Region n, [(Region n, Vec n R)]] -> Maybe [(Matrix m n, Vec m (VSet n))]

