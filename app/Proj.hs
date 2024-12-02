
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

module Proj where

import FixedVector
import Elvis
import Data.Singletons

precision_ :: Num a => a
precision_ = 25

-- Exponetially searches until past edge of the set
approx_find_ :: (RealVec (Vec n) a) => a -> (Vec n a -> a) -> Vec n a -> Vec n a
approx_find_ b f v 
    | f (b |*| v) > 0 = approx_ 2 f v (b/2)
    | otherwise = approx_find_ (b*2) f v

-- Binary search to find edge of set once past
approx_ :: (RealVec (Vec n) a) => Integer -> (Vec n a -> a) -> Vec n a -> a -> Vec n a
approx_ b f v p
    | b >= precision_ = v
    | f (p|*|v) > 0 = approx_  (b+1) f (v |-| v_prime) (p/2)
    | f (p|*|v) < 0 = approx_  (b+1) f (v |+| v_prime) (p/2)
    | otherwise = v
        where v_prime = (p/2) |*| v

-- Public wrapper
approx :: (RealVec (Vec n) a) => (Vec n a -> a) -> Vec n a -> Vec n a
approx f v = (approx_find_ 1 f v)

-- Diagonal center bisector for given set
directional_identification :: (RealVec (Vec n) a, SingI n) => (Vec n a -> a) -> Vec n a
directional_identification f = v where
    positive_diag = foldr (|+|) zeroVecs (generate (\i -> approx f (index i baseVecs)))
    negative_diag = foldr (|+|) zeroVecs ((zeroVecs|-|) <$> (generate (\i -> approx f (index i baseVecs))))
    v = positive_diag |-| negative_diag

test_func :: Vec (Lit 2) Float -> Float
test_func v = (index (FZ) v)**2 + (index (FS FZ) v)**2 - 1

test_v :: Vec (Lit 2) Float
test_v = 1:#1:#Nil

ds :: (RealVec (Vec n) a) => Vec n a -> Vec n a -> a
ds x0 x = norm $ x |-| x0

