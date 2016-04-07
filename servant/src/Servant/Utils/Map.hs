{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Servant.Utils.Map (mapLeaves, StripIdentity(..)) where

import           Data.Functor.Identity

import           Servant.API

class Mappable s t a b where
  mapLeaves :: (forall x . a x -> b x) -> s -> t

instance (Mappable left left' a b, Mappable right right' a b) => Mappable (left :<|> right) (left' :<|> right') a b where
  mapLeaves f (left :<|> right) = mapLeaves f left :<|> mapLeaves f right

instance Mappable s t a b => Mappable (arg -> s) (arg -> t) a b where
  mapLeaves f s = mapLeaves f . s

instance Mappable (a x) (b x) a b where
  mapLeaves f a = f a

class StripIdentity a where
  type Stripped a :: *
  stripIdentity :: a -> Stripped a

instance (StripIdentity left, StripIdentity right) => StripIdentity (left :<|> right) where
  type Stripped (left :<|> right) = Stripped left :<|> Stripped right
  stripIdentity (left :<|> right) = stripIdentity left :<|> stripIdentity right

instance StripIdentity b => StripIdentity (a -> b) where
  type Stripped (a -> b) = a -> Stripped b
  stripIdentity f = \ a -> stripIdentity (f a)

instance StripIdentity (Identity a) where
  type Stripped (Identity a) = a
  stripIdentity (Identity a) = a
