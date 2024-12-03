
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
import Data.Number.CReal

type R = CReal

precision :: Num a => a
precision = 8

zeroVecs_ :: RealFloat a => Sing n -> Vec n a
zeroVecs_ = \case
    SZ -> Nil
    SS l -> 0:# zeroVecs_ l

zeroVecs :: (RealFloat a, SingI n) => Vec n a
zeroVecs = zeroVecs_ sing

baseVecs_ :: RealFloat a => Sing n -> Vec n (Vec n a)
baseVecs_ = \case
    SZ -> Nil
    SS l -> (1 :# zeroVecs_ l) :# ((0 :#) <$> baseVecs_ l)

baseVecs :: (RealFloat a, SingI n) => Vec n (Vec n a)
baseVecs = baseVecs_ sing

negativeVecs_ :: (RealFloat a) => Sing n -> Vec n (Vec n a)
negativeVecs_ = \case
    SZ -> Nil
    SS l -> (-1 :# zeroVecs_ l) :# ((0 :#) <$> negativeVecs_ l)

negativeVecs :: (RealFloat a, SingI n) => Vec n (Vec n a)
negativeVecs = negativeVecs_ sing

class (RealFloat a, Foldable v, Applicative v) => RealVec v a where  
    norm :: v a -> a 
    (<.>) :: v a -> v a -> a
    (|+|) :: v a -> v a -> v a
    (|-|) :: v a -> v a -> v a
    (|*|) :: a -> v a -> v a
    unit :: v a -> v a
    bisector :: v a -> v a -> v a
    getAngle :: v a -> v a -> a
    dirDerivative :: (v a -> a) -> v a -> v a -> a
    grad :: (v a -> a) -> v a -> v a

norm_ :: (RealFloat a) => Sing n -> Vec n a -> a
norm_ = \case
        SZ -> \_ -> 0
        SS l -> \(x:#xs) -> x**2 + norm_ l xs
    
dot_ :: (RealFloat a) => Sing n -> Vec n a -> Vec n a -> a
dot_ = \case
    SZ -> \_ -> \_ -> 0
    SS l -> \(x:#xs) -> \(y:#ys) -> (x*y) + dot_ l xs ys

add_ :: (RealFloat a) => Sing n -> Vec n a -> Vec n a -> Vec n a
add_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \(x:#xs) -> \(y:#ys) -> (x+y) :# add_ l xs ys

sub_ :: (RealFloat a) => Sing n -> Vec n a -> Vec n a -> Vec n a
sub_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \(x:#xs) -> \(y:#ys) -> (x-y) :# sub_ l xs ys

scmult_ :: (RealFloat a) => Sing n -> a -> Vec n a -> Vec n a
scmult_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \r -> \(x:#xs) -> (r*x) :# scmult_ l r xs

instance (RealFloat a, SingI (n::Nat), Applicative (Vec n)) => RealVec (Vec n) a where
    norm v = norm_ sing v
    (<.>) x y = dot_ sing x y
    (|+|) x y = add_ sing x y
    (|-|) x y = sub_ sing x y
    (|*|) r x = scmult_ sing r x
    unit x = (1/(norm x)) |*| x
    bisector x y = ((norm y) |*| x) |+| ((norm x) |*| y)
    getAngle x y = acos ((x<.>y) / (norm x * norm y))
    dirDerivative f x v = (f(x |+| (h|*|v)) - f(x)) / h  where
         h = 10 ** (-precision)
    grad f x = generate (\i -> dirDerivative f x (index i baseVecs))

type ConFun n a = [(Vec n a -> a)]
type VSet m n a = Vec m (ConFun n a)


class CSet m n where
    contains :: (RealFloat a) => Vec n a -> VSet m n a -> Bool
    intersection :: (RealFloat a, CSet k n) => VSet k n a -> VSet m n a -> VSet (k+m) n a
    add :: (RealFloat a, CSet k n) => VSet m n a -> VSet k n a -> VSet (m*k) n a
    proj :: (RealFloat a) => VSet m n a -> Vec n a -> Vec n a
    distance :: (RealFloat a) => Vec n a -> VSet m n a -> a

contains_add :: (RealFloat a) => [a] -> a
contains_add [] = 0
contains_add (x:xs) = min x (contains_add xs)

contains_inter :: (RealFloat a) => (Sing n, Sing m) -> Vec n a -> VSet m n a -> Bool
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


ellipsoidf :: (RealFloat a) => a -> Vec n a -> Vec n a -> a
ellipsoidf r Nil Nil = r**2
ellipsoidf r (c:#cs) (x:#xs) = x**2 / c**2 + ellipsoidf r cs xs


ballf :: (RealFloat a) => a -> Vec n a -> a
ballf r Nil = r**2
ballf r (x:#xs) = x**2 + ballf r xs

ball :: (SingI n, RealVec (Vec n) a) => a -> VSet (Lit 1) n a
ball r = [ballf r]:#Nil
    
ellipsoid :: (SingI n, RealVec (Vec n) a) => a -> Vec n a -> VSet (Lit 1) n a
ellipsoid r c = [ellipsoidf r c]:#Nil
