
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

precision_ :: R
precision_ = 25

recentered_approx :: (RealVec (Vec n R), SingI n) => (Vec n R -> R) -> Vec n R -> Vec n R
recentered_approx g x = up (approx g_prime x_prime) where
    x_prime = down x
    (up, down, g_prime) = recenter g

-- Exponetially searches until past edge of the set
approx_find_ :: (RealVec (Vec n R)) => (Vec n R -> R) -> Vec n R -> Vec n R
approx_find_ f v 
    | f v > 0 = approx_ 0 f v 1
    | f v == 0 = v
    | otherwise = approx_find_ f (2 |*| v)

-- Binary search to find edge of set once past
approx_ :: (RealVec (Vec n R)) => R -> (Vec n R -> R) -> Vec n R -> R -> Vec n R
approx_ b f v p
    | b >= precision_ = v
    | f (v) > 0 = approx_  (b+1) f (v |-| ((p/2) |*| v)) (p/2)
    | f (v) < 0 = approx_  (b+1) f (v |+| ((p/2) |*| v)) (p/2)
    | otherwise = v
        
-- Public wrapper
approx :: (RealVec (Vec n R)) => (Vec n R -> R) -> Vec n R -> Vec n R
approx f v = (approx_find_ f v)

positive_diag :: (RealVec (Vec n R), SingI n) => (Vec n R -> R) -> (Vec n R)
positive_diag f = foldr (|+|) zeroVecs (generate (\i -> approx f (index i baseVecs)))

negative_diag :: (RealVec (Vec n R), SingI n) => (Vec n R -> R) -> (Vec n R)
negative_diag f = foldr (|+|) zeroVecs (generate (\i -> approx f (index i negativeVecs)))

-- Diagonal center bisector for given set
directional_identification :: (RealVec (Vec n R), SingI n) => (Vec n R -> R) -> Vec n R
directional_identification f = v where
    x = positive_diag f
    y = negative_diag f
    z = x |-| y
    v = x |-| ((1/2) |*| z)


test_func :: Vec (Lit 2) R -> R
test_func v = (index (FZ) v)^2 + (((index (FS FZ) v) + 1)^2)/4 - 1

test_v :: Vec (Lit 2) R
test_v = 1:#1:#Nil

--proj :: (RealVec (Vec n) a, SingI n) => (Vec n a -> a) -> Vec n a -> Vec n a

recenter :: (RealVec (Vec n R), SingI n) => (Vec n R -> R) -> ((Vec n R -> Vec n R), (Vec n R -> Vec n R), (Vec n R -> R))
recenter f = (to_center, from_center, g) where
    c = directional_identification f
    to_center v =  (v |+| c)
    from_center v = (v |-| c)
    g v = f (v |+| c)


-- r is a boundary vector as above, x is the vector which we wish to find the projection of
-- delta f is the distance function from x_bar to r_bar, and f is the distance functionName

delf :: (RealVec (Vec n R), SingI n) => (Vec n R) -> (Vec n R) -> (Vec n R -> R)
delf r x = ball_adjust (norm (x|-|r)) where
    ball_adjust a b = ballf a (b|-|x)

toward_y :: (RealVec (Vec n R), SingI n) => (Vec n R -> Vec n R) -> Vec n R -> Vec n R -> Vec n R
toward_y grad_g r x = (unit $ (grad (delf r x)) r) |+| (unit $ grad_g r)

-- there exists some episilon s.t. -episilon * (toward_y g r x) + r = y 

-- private wrapper
-- x is assumed to be recentered
estimate_y :: (RealVec (Vec n R), SingI n) => (Vec n R -> R) -> Vec n R -> Vec n R
estimate_y g x = estimate_y_find_ 0 g r x where
    r = approx g x

estimate_y_find_ :: (RealVec (Vec n R), SingI n) => R -> (Vec n R -> R) -> Vec n R -> Vec n R -> Vec n R
estimate_y_find_ episilon g y0 x
    | theta_plus == (pi/2) = y_plus
    | theta_minus == (pi/2) = y_minus
    | theta_minus == theta_plus = y0
    | theta_plus > theta_minus = estimate_y_find_ (episilon + 1) g y_plus x
    | theta_minus > theta_plus = estimate_y_ 0 (2 ** (episilon-1)) g y0 x
    | otherwise = y0 where
        mult_factor = 2 ** episilon
        theta_plus = getAngle (grad_g y_plus) y_plus
        theta_minus = getAngle (grad_g y_minus) y_minus
        y_plus = approx g (y0 |+| (mult_factor |*| (toward_y grad_g y0 x)))
        y_minus = approx g (y0 |-| (mult_factor |*| (toward_y grad_g y0 x)))
        grad_g = grad g
    

estimate_y_ :: (RealVec (Vec n R), SingI n) => R -> R -> (Vec n R -> R) -> Vec n R -> Vec n R -> Vec n R 
estimate_y_ b episilon g y0 x
    | b >= precision_ = y0
    | theta_plus == (pi / 2) = y_plus
    | theta_minus == (pi/2) = y_minus
    | theta_minus == theta_plus = y0
    | theta_plus > theta_minus = estimate_y_ (b+1) (episilon/2) g y_plus x
    | theta_minus > theta_plus = estimate_y_ (b+1) (episilon/2) g y_minus x
    | otherwise = y0 where
        theta_plus = getAngle (grad_g y_plus) y_plus
        theta_minus = getAngle(grad_g y_minus) y_minus
        grad_g = grad g
        y_plus = approx g (y0 |+| (episilon |*| (toward_y grad_g y0 x)))
        y_minus = approx g (y0 |-| (episilon |*| (toward_y grad_g y0 x)))

-- x, g are assumed to be uncentered - this is the public wrapper
single_proj :: (RealVec (Vec n R), SingI n) => (Vec n R -> R) -> Vec n R -> Vec n R
single_proj g x = y where
    y = up $ estimate_y (g_centered) (down x)
    (up, down, g_centered) = recenter g