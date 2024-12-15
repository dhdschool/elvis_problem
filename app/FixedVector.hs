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
{-# LANGUAGE MultiParamTypeClasses #-}

-- An arrow with both direction and magnitude

module FixedVector where

import qualified GHC.TypeLits
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TH
import           Data.Singletons.Base.TH
import Data.Maybe (fromJust)
import Unsafe.Coerce



-- This is template haskell, the official haskell metaprogramming language
-- This defines singleton types for all natural numbers at compile time
$(singletons [d|
  data Nat = Z | S Nat
    deriving Eq    
  |])

instance Show Nat where
    show Z = "Z"
    show (S n) = "S " ++ show n

fromIntegerNat :: (Integral b) => b -> Nat

fromIntegerNat 0 = Z
fromIntegerNat n = S (fromIntegerNat (n-1))

-- This will return a runtime error if provided a negative integer to represent a nat

fromIntegerUnsafe :: (Integral b) => b -> SNat n
fromIntegerUnsafe n
    | n < 0 = error "Negative size provided to a natural type-level number"
    | n == 0 = unsafeCoerce SZ
    | otherwise = unsafeCoerce $ SS (fromIntegerUnsafe (n-1))
    
instance Show (SNat n) where
    show SZ = "SZ"
    show (SS l) = "SS " ++ show l 

-- instance Num (SNat 'Z) where
--     fromInteger _ = SZ

-- instance (Num (SNat n)) => Num (SNat ('S n)) where
--     fromInteger a = SS (fromInteger (a - 1))

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

pure_ :: Sing n -> a -> Vec n a
pure_ = \case
    SZ -> \_ -> Nil
    SS l -> \x -> x :# (pure_ l x)

liftA_ :: Sing n -> Vec n (a -> b) -> Vec n a -> Vec n b
liftA_ = \case
    SZ -> \_ -> \_ -> Nil
    SS l -> \(fv:#fvs) -> \(v:#vs) -> fv v :# (liftA_ l fvs vs)

instance (SingI n) => Applicative (Vec n) where
    pure :: a -> Vec n a
    pure x = pure_ sing x

    (<*>) :: Vec n (a -> b) -> Vec n a -> Vec n b
    (<*>) fv v = liftA_ sing fv v
-- instance Applicative (Vec Z) where
--     pure _ = Nil
--     (<*>) _ _ = Nil

-- instance (Applicative (Vec n)) => Applicative (Vec ('S n)) where
--     pure :: Applicative (Vec n) => a -> Vec (S n) a
--     pure x = x :# pure x

--     (<*>) :: Applicative (Vec n) => Vec (S n) (a -> b) -> Vec (S n) a -> Vec (S n) b
--     (<*>) (fv:#fvs) (v:#vs) = fv v :# (fvs <*> vs) 

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

-- Takes a vector where each element is a pair and returns two vectors
unzipVec :: Vec n (a, b) -> (Vec n a, Vec n b)
unzipVec = \case
    Nil -> (Nil, Nil)
    ((a, b):#vs) -> ((a:#as), (b:#bs)) where
        (as, bs) = unzipVec vs

unzipVec3 :: Vec n (a, b, c) -> (Vec n a, Vec n b, Vec n c)
unzipVec3 = \case
    Nil -> (Nil, Nil, Nil)
    ((a, b, c):#vs) -> ((a:#as), (b:#bs), (c:#cs)) where
        (as, bs, cs) = unzipVec3 vs


--Defining addition and subtraction on the type level for successor nats
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

getFirst :: Vec (S n) a -> a
getFirst = \case
    (x:#_) -> x

getLast :: Vec (S n) a -> a
getLast = \case
    (x:#Nil) -> x
    (_:#xs) -> getLast_ xs

getLast_ :: (Vec n a -> a)
getLast_ = \case 
    (x:#Nil) -> x
    (_:#xs) -> getLast_ xs

vecTail :: (SingI (S n)) => Vec (S n) a -> Vec n a
vecTail x = vecTail_ sing x

vecTail_ :: Sing (S n) -> Vec (S n) a -> Vec n a
vecTail_ = \case
    SS SZ -> \_ -> Nil
    SS (SS l) -> \(x:#xs) -> x :# (vecTail_ (SS l) xs)

reverseTail_ :: Sing (S n) -> Vec (S n) a -> Vec n a
reverseTail_ = \case
    SS SZ -> \(_:#Nil) -> Nil
    SS (SS l) -> \(x:#xs) -> x :# (reverseTail_ (SS l) xs)

reverseTail :: (SingI (S n)) => Vec (S n) a -> Vec n a
reverseTail x = reverseTail_ sing x 

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


-- Use these functions with care, they aren't guaranteed to return you a vector because a list may be of any size
-- whereas a particular vector may be of only one size

-- This function is empirically determined to be safe at runtime as lists cannot have negative sizes
-- and fromIntegerUnsafe is only unsafe for negative integers

fromListSafe :: [a] -> (Vec n a, Sing n)
fromListSafe lst = (fromJust $ fromListExplicit size lst, size) where
    size = fromIntegerUnsafe (length lst)

-- Return a vector of a given size if given a list of that size, otherwise return Nothing
fromListExplicit :: Sing n -> [a] -> Maybe ((Vec n a))
fromListExplicit = \case
    SZ -> \case
        [] -> Just Nil
        _ -> Nothing

    SS l -> \case
        [] -> Nothing
        (x:xs) -> (x:#) <$> (fromListExplicit l xs)

-- Returns a vector of implicit size if a given list is of that size, otherwise return Nothing
fromList :: (SingI n) => [a] -> Maybe (Vec n a)
fromList x = fromListExplicit sing x

vecPairs_ :: Sing (S n) -> (a -> a -> a) -> Vec (S n) a -> Vec n a
vecPairs_ = \case
    SS SZ -> \_ -> \_ -> Nil
    SS (SS l) -> \f -> \(x:#(x2:#xs)) -> (f x2 x) :# vecPairs_ (SS l) f (x2:#xs)
 
vecPairs :: (SingI (S n)) => (a -> a -> a) -> (Vec (S n) a) -> Vec n a
vecPairs f x = vecPairs_ sing f x 