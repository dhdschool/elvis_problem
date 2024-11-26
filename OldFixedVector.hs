
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators, MultiParamTypeClasses, ConstraintKinds, ScopedTypeVariables, UndecidableInstances, FlexibleContexts, FlexibleInstances, StandaloneDeriving, IncoherentInstances#-} 
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

--module FixedVector where

import Data.Data hiding (Proxy)
import Data.Kind
import GHC.TypeLits ( type (+), type (-), CmpNat, Nat )

data Proxy a = Proxy

data Vec :: Nat -> Data.Kind.Type -> Data.Kind.Type where
    Nil  :: Vec 0 a
    (:#) :: a -> Vec (n - 1) a -> Vec n a

infixr 5 :#

deriving instance Show a => Show (Vec n a)
deriving instance Eq a => Eq (Vec n a)
deriving instance Functor (Vec a)
deriving instance Foldable (Vec a)
deriving instance Traversable (Vec a)


type x > y = CmpNat x y ~ 'GT
type x == y = CmpNat x y ~ 'EQ

instance Applicative (Vec 0) where
    pure _    = Nil
    Nil <*> _ = Nil

instance (Applicative (Vec (n - 1)), n > 0) => Applicative (Vec n) where
    pure x = x :# pure x
    (f :# fs) <*> (x :# xs) = f x :# (fs <*> xs)


class Index (n :: Nat) v where
    index :: Proxy n -> v a -> a

instance (m > 0) => Index 0 (Vec m) where
    index _ (x :# _) = x

instance forall n m. (Index (n - 1) (Vec (m - 1)), n > 0, m > 0) => Index n (Vec m) where
    index _ (_ :# xs) = index (Proxy :: Proxy (n - 1)) xs

class Unfoldable v where
    unfold :: (b -> (a, b)) -> b -> v a
instance Unfoldable (Vec 0) where
    unfold _ _ = Nil

instance forall n. (Unfoldable (Vec (n-1))) => Unfoldable (Vec n) where
    unfold f x0 = let (y, x1) = f x0 in y :# unfold f x1



fromListMaybes :: Unfoldable v => [a] -> v (Maybe a)
fromListMaybes = unfold $ \l -> case l of
                                    []   -> (Nothing, [])
                                    x:xs -> (Just x , xs)

fromList :: (Unfoldable v, Traversable v) => [a] -> Maybe (v a)
fromList = sequence . fromListMaybes



appendV :: forall (n :: Nat) a (m :: Nat). Vec n a -> Vec m a -> Vec (n + m) a
appendV Nil ys = ys
appendV (x:#xs) ys = x :# appendV xs ys