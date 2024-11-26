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

-- You don't even want to know how many hours it took to get a suitable version of this
-- Certainly one of my longest math side quests

module FixedVector where

import qualified GHC.TypeLits
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TH
import           Data.Singletons.Base.TH


    
$(singletons [d|
  data Nat = Z | S Nat
    deriving Eq
  |])

type family Lit (n :: GHC.TypeLits.Nat) :: Nat where
    Lit 0 = Z
    Lit n = S (Lit (n GHC.TypeLits.- 1)) 


data Fin :: Nat -> Type where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)
deriving instance Show (Fin n)

data Vec :: Nat -> Type -> Type where
    Nil :: Vec 'Z a
    (:#) :: a -> Vec n a -> Vec ('S n) a

instance (Show a) => Show (Vec n a) where
    show Nil = "Nil\n"
    show (x:#xs) = (show x) ++ (":#") ++ (show xs)

infixr 5 :#

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


zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec = \case
    Nil -> \case
        Nil -> Nil
    x:#xs -> \case
        y:#ys -> (x, y) :# zipVec xs ys

type family (n::Nat) + (m::Nat) :: Nat where
    'Z + m = m
    'S n + m = 'S (n+m)

type family (n::Nat) - (m::Nat) :: Nat where
    'S n - m = 'S(n-m)

type family (n::Nat) * (m::Nat) :: Nat where
    n * m = Lit ( (IntNat n) GHC.TypeLits.* (IntNat m))
    
type family IntNat (n::Nat) :: GHC.TypeLits.Nat where
    IntNat Z = 0
    IntNat (S n) = 1 GHC.TypeLits.+ (IntNat n)


(|++|) :: Vec n a -> Vec m a -> Vec (n+m) a
(|++|) = \case
    Nil -> \ys -> ys
    x:#xs -> \ys -> x :# (xs |++| ys)

index :: Fin n -> Vec n a -> a
index = \case
    FZ -> \case
        x:#_ -> x
    FS i -> \case
        _ :# xs -> index i xs

vecreplicate_ :: Sing n -> a -> Vec n a
vecreplicate_ = \case
    SZ -> \_ -> Nil
    SS l -> \x -> x :# vecreplicate_ l x

vecreplicate :: SingI n => a -> Vec n a
vecreplicate = vecreplicate_ sing

generate_ :: Sing n -> (Fin n -> a) -> Vec n a
generate_ = \case
    SZ   -> \_ -> Nil
    SS l -> \f -> f FZ :# generate_ l (f . FS)

generate :: SingI n => (Fin n -> a) -> Vec n a
generate = generate_ sing
