
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

construct_graph :: (SingI n) => VelocityRegions n -> Vec n R -> Vec n R -> Graph n
construct_graph regions x0 x1 = construct_graph_ HashMap.empty (x0, x1) (region_from_points regions x0)

get_velocity_set :: (SingI n) => Region n -> VelocityRegions n -> Maybe (VSet n)
get_velocity_set region rmap = HashMap.lookup region rmap


search_graph :: (SingI n) => Region n -> Graph n -> (Maybe (Node n))
search_graph search graph = HashMap.lookup search graph

get_all_paths_ :: (SingI n) => Node n -> VelocityRegions n -> Region n -> Region n -> Graph n -> Maybe [[(Vec n R, VSet n, Region n)]]
get_all_paths_ [] _ _ _ _ = Just [[]]

get_all_paths_ ((next_region, interface_vector):interface_tail) regions current_region end_region graph 
    | next_region == end_region = case (get_velocity_set next_region regions) of
        Nothing -> Nothing
        Just vset -> case (maybe_width) of 
            Nothing -> Nothing
            Just width -> Just ([[(interface_vector, vset, next_region)]] ++ width)
    | otherwise = case (get_velocity_set current_region regions) of
        Nothing -> Nothing
        Just vset -> case (maybe_depth, maybe_width) of
            (Just depth, Just width) -> Just (((node:) <$> depth) ++ (width)) where
                node = (interface_vector, vset, current_region)
            (_, _) -> Nothing
    where 
        maybe_depth = case maybe_node of
            Nothing -> Nothing
            Just node_val -> get_all_paths_ node_val regions next_region end_region graph
        maybe_width = get_all_paths_ interface_tail regions current_region end_region graph
        maybe_node = (search_graph next_region graph)


get_all_paths :: (SingI n) => VelocityRegions n -> Region n -> Region n -> Graph n -> Maybe [[(Vec n R, VSet n, Region n)]]
get_all_paths regions start_region end_region graph = case maybe_vset of
    Nothing -> Nothing
    Just node ->  get_all_paths_ node regions start_region end_region graph
    where
        maybe_vset = (search_graph start_region graph) 





