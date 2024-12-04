
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
import Data.Singletons
import Data.Kind (Type)
import Data.Foldable
import Data.Number.BigFloat

type R = BigFloat Prec10

precision :: R
precision = 8

zeroVecs_ :: Sing n -> Vec n R
zeroVecs_ = \case
    SZ -> Nil
    SS l -> 0:# zeroVecs_ l

zeroVecs :: (SingI n) => Vec n R
zeroVecs = zeroVecs_ sing

baseVecs_ :: Sing n -> Vec n (Vec n R)
baseVecs_ = \case
    SZ -> Nil
    SS l -> (1 :# zeroVecs_ l) :# ((0 :#) <$> baseVecs_ l)

baseVecs :: (SingI n) => Vec n (Vec n R)
baseVecs = baseVecs_ sing

negativeVecs_ :: Sing n -> Vec n (Vec n R)
negativeVecs_ = \case
    SZ -> Nil
    SS l -> (-1 :# zeroVecs_ l) :# ((0 :#) <$> negativeVecs_ l)

negativeVecs :: (SingI n) => Vec n (Vec n R)
negativeVecs = negativeVecs_ sing

class RealVec v where  
    norm :: v -> R 
    (<.>) :: v -> v -> R
    (|+|) :: v -> v -> v
    (|-|) :: v -> v -> v
    (|*|) :: R -> v -> v
    unit :: v -> v
    bisector :: v -> v -> v
    getAngle :: v -> v -> R
    dirDerivative :: (v -> R) -> v -> v -> R
    grad :: (v -> R) -> v -> v

norm_ :: Sing n -> Vec n R -> R
norm_ = \case
        SZ -> \_ -> 0
        SS l -> \(x:#xs) -> x^2 + norm_ l xs
    
dot_ :: Sing n -> Vec n R -> Vec n R -> R
dot_ = \case
    SZ -> \_ -> \_ -> 0
    SS l -> \(x:#xs) -> \(y:#ys) -> (x*y) + dot_ l xs ys

add_ :: Sing n -> Vec n R -> Vec n R -> Vec n R
add_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \(x:#xs) -> \(y:#ys) -> (x+y) :# add_ l xs ys

sub_ :: Sing n -> Vec n R -> Vec n R -> Vec n R
sub_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \(x:#xs) -> \(y:#ys) -> (x-y) :# sub_ l xs ys

scmult_ :: Sing n -> R -> Vec n R -> Vec n R
scmult_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \r -> \(x:#xs) -> (r*x) :# scmult_ l r xs

instance (SingI (n::Nat), Applicative (Vec n)) => RealVec (Vec n R) where
    norm v = sqrt $ norm_ sing v
    (<.>) x y = dot_ sing x y
    (|+|) x y = add_ sing x y
    (|-|) x y = sub_ sing x y
    (|*|) r x = scmult_ sing r x
    unit x = (1/(norm x)) |*| x
    bisector x y = ((norm y) |*| x) |+| ((norm x) |*| y)
    getAngle x y = acos ((x<.>y) / ( (norm x) * (norm y)) )
    dirDerivative f x v = (f(x |+| (h|*|v)) - f(x)) / h  where
         h = (10::R) ** (-precision)
    grad f x = generate (\i -> dirDerivative f x (index i baseVecs))

type ConFun n = [(Vec n R -> R)]
type VSet m n = Vec m (ConFun n)


class CSet m n where
    contains :: Vec n R -> VSet m n -> Bool
    intersection :: (CSet k n) => VSet k n -> VSet m n -> VSet (k+m) n
    add :: (CSet k n) => VSet m n -> VSet k n -> VSet (m*k) n
    proj :: VSet m n  -> Vec n R -> Vec n R
    distance :: Vec n R -> VSet m n -> R

contains_add :: [R] -> R
contains_add [] = 0
contains_add (x:xs) = min x (contains_add xs)

contains_inter :: (Sing n, Sing m) -> Vec n R -> VSet m n -> Bool
contains_inter = \case
    (SZ, _)  -> \_ -> \_ -> True
    (_, SZ) -> \_ -> \_ -> False  
    (l1, SS l2) -> \v -> \(f:#fs) -> ((contains_add $ f <*> (pure v)) <=0) && contains_inter (l1, l2) v fs


--distance_ :: (RealFloat a) => VSet m n a -> Vec n a -> a


instance (SingI m, SingI n) => CSet m n where
    contains v f = contains_inter (sing, sing) v f
    intersection f1 f2 = (f1 |++| f2)
    --distance 
    --add f1 f2 = f1 <*> ((++) <$> f2)


-- ellipsoidf :: R -> Vec n R -> Vec n R -> R
-- ellipsoidf r Nil Nil = r^2
-- ellipsoidf r (c:#cs) (x:#xs) = x^2 / c^2 + ellipsoidf r cs xs


ballf :: (RealVec (Vec n R)) => R -> Vec n R -> R
ballf r x = norm x - r

ball :: (SingI n, RealVec (Vec n R)) => R -> VSet (Lit 1) n
ball r = [ballf r]:#Nil
    
-- ellipsoid :: (SingI n, RealVec (Vec n R)) => R -> Vec n R -> VSet (Lit 1) n
-- ellipsoid r c = [ellipsoidf r c]:#Nil
