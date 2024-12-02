
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

approx_ :: (RealVec (Vec n) a) => Integer -> (Vec n a -> a) -> Vec n a -> a -> a
approx_ b f v p
    | b >= precision_ = 0
    | f (p|*|v) > 0 = approx_  (b+1) f v (p - dp) - (dp)
    | f (p|*|v) < 0 = approx_  (b+1) f v (p + dp) + (dp)
    | otherwise = 0 where
        dp = (1/(2^b))

approx :: (RealVec (Vec n) a) => (Vec n a -> a) -> Vec n a -> a
approx f v = (1/2) + approx_ 2 f v (1/2)

test_func :: Vec (Lit 2) Float -> Float
test_func v = (index (FZ) v)**2 + (index (FS FZ) v)**2 - 1

test_v :: Vec (Lit 2) Float
test_v = 1:#1:#Nil



set_edge :: (RealVec (Vec n) a) => (Vec n a -> a) -> Vec n a -> Vec n a
set_edge f v = (approx (f) v) |*| v 

outer_distance :: (RealVec (Vec n) a) => (Vec n a -> a) -> Vec n a -> a
outer_distance f v = norm $ v |-| (set_edge f v)

--theta :: (RealVec (Vec n) a) => Vec n a -> a
--theta v = v <.> (index)

-- proj = v - r cos(alpha)
-- distance = 2 * sin(theta) * cos(theta) * norm(v - r)
-- alpha = (pi / 2) - 2 * theta
-- theta = (v <.> e1) / (norm v)