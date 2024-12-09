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
{-# LANGUAGE DeriveFoldable #-}

-- An arrow with both direction and magnitude

module FixedVector where

import qualified GHC.TypeLits
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TH
import           Data.Singletons.Base.TH
import Data.Number.BigFloat


-- This is template haskell, the official haskell metaprogramming language
-- This defines singleton types for all natural numbers at compile time
$(singletons [d|
  data Nat = Z | S Nat
    deriving Eq
  |])

-- This is a conversion between integer literals and their successor natural equivalents on the type level
type family Lit (n :: GHC.TypeLits.Nat) :: Nat where
    Lit 0 = Z
    Lit n = S (Lit (n GHC.TypeLits.- 1)) 

type Dim n = Fin (Lit n)

-- Takes in a size and an integer and returns a finite literal, will default to returning 0 if an invalid integer is given
dim_ :: Sing n -> Integer -> Fin (S n)
dim_ SZ 0 = FZ
dim_ (SS l) n = FS (dim_ (l) (n-1))
dim_ _ _ = FZ

-- Implicit sized finite literal from number
dim :: (SingI n) => Integer -> Fin (S n)
dim n = dim_ sing n

-- Finite types, any valid natural between 0 and a nat n is a Finite of n
data Fin :: Nat -> Type where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)
deriving instance Show (Fin n)


-- Vector GADT definition, Nil is the end of the vector and :# is the construction operator
data Vec :: Nat -> Type -> Type where
    Nil :: Vec 'Z a
    (:#) :: a -> Vec n a -> Vec ('S n) a

instance (Show a) => Show (Vec n a) where
    show Nil = "Nil\n"
    show (x:#xs) = (show x) ++ (":#") ++ (show xs)

infixr 5 :#

--Below are Haskell typeclasses that provide extremely useful operations such as fmap and lift
--These operations are used extensively in the other files

instance Functor (Vec n) where
    fmap _ Nil = Nil
    fmap f (x:#xs) = f x :# (f <$> xs)

instance Applicative (Vec Z) where
    pure _ = Nil
    (<*>) _ _ = Nil

instance (Applicative (Vec n)) => Applicative (Vec ('S n)) where
    pure :: Applicative (Vec n) => a -> Vec (S n) a
    pure x = x :# pure x

    (<*>) :: Applicative (Vec n) => Vec (S n) (a -> b) -> Vec (S n) a -> Vec (S n) b
    (<*>) (fv:#fvs) (v:#vs) = fv v :# (fvs <*> vs) 

deriving instance Foldable (Vec n)
instance (Eq a) =>  Eq (Vec n a) where
    (==) Nil Nil = True
    (==) (x:#xs) (y:#ys) = (x==y) && (xs == ys)

-- Takes two vectors and returns one vector where each element is a pair
zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec = \case
    Nil -> \case
        Nil -> Nil
    x:#xs -> \case
        y:#ys -> (x, y) :# zipVec xs ys

-- Defining addition and subtraction on the type level for successor nats
type family (n::Nat) + (m::Nat) :: Nat where
    'Z + m = m
    'S n + m = 'S (n+m)

type family (n::Nat) - (m::Nat) :: Nat where
    'S n - m = 'S(n-m)

-- Vector append operation
(|++|) :: Vec n a -> Vec m a -> Vec (n+m) a
(|++|) = \case
    Nil -> \ys -> ys
    x:#xs -> \ys -> x :# (xs |++| ys)

-- Vector indexing using a valid finite
index :: Fin n -> Vec n a -> a
index = \case
    FZ -> \case
        x:#_ -> x
    FS i -> \case
        _ :# xs -> index i xs

-- Generating a vector of size n where every value is a given scalar
vecreplicate_ :: Sing n -> a -> Vec n a
vecreplicate_ = \case
    SZ -> \_ -> Nil
    SS l -> \x -> x :# vecreplicate_ l x

-- Implictily sized vector where every value is a given scalar
vecreplicate :: SingI n => a -> Vec n a
vecreplicate = vecreplicate_ sing

-- Generating a vector of size n from a function of finites
-- This can be used to convert a list of size n to a vector, for example
generate_ :: Sing n -> (Fin n -> a) -> Vec n a
generate_ = \case
    SZ   -> \_ -> Nil
    SS l -> \f -> f FZ :# generate_ l (f . FS)

-- Implicitly sized vector from a function of implicitly dimensioned finites
generate :: SingI n => (Fin n -> a) -> Vec n a
generate = generate_ sing

-- A type alias for the real numbers, this defines them as float with 10 decimal places of precision
type R = BigFloat Prec10

precision :: Integer
precision = 8

-- A zero vector of size n
zeroVecs_ :: Sing n -> Vec n R
zeroVecs_ = \case
    SZ -> Nil
    SS l -> 0:# zeroVecs_ l

-- A zero vector that has an implicit size of n using haskell's powerfull type system
-- For example, if you had (3:#2:#Nil) + zeroVecs, you would not have to define the size of
-- zeroVecs because addition is only defined between vectors of the same size,
-- so Haskell assumes that the size of the zeroVec is 2

zeroVecs :: (SingI n) => Vec n R
zeroVecs = zeroVecs_ sing

--Identity matrix of size n x n
baseVecs_ :: Sing n -> Vec n (Vec n R)
baseVecs_ = \case
    SZ -> Nil
    SS l -> (1 :# zeroVecs_ l) :# ((0 :#) <$> baseVecs_ l)

--Identity square matrix of implicit size
baseVecs :: (SingI n) => Vec n (Vec n R)
baseVecs = baseVecs_ sing

-- Negative identity matrix of size n x n
negativeVecs_ :: Sing n -> Vec n (Vec n R)
negativeVecs_ = \case
    SZ -> Nil
    SS l -> (-1 :# zeroVecs_ l) :# ((0 :#) <$> negativeVecs_ l)

--Negative identity square matrix of implicit size
negativeVecs :: (SingI n) => Vec n (Vec n R)
negativeVecs = negativeVecs_ sing
