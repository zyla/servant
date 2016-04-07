{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
module Servant.Utils.Map (mapLeaves, StripIdentity(..)) where

import           Data.Functor.Identity
import           Data.Proxy

import           Servant.API

mapLeaves :: forall s t a b . (Mappable s t a b, StripIdentity t) =>
  (forall x . a x -> b x) -> s -> Proxy t -> Stripped t
mapLeaves f s _ = stripIdentity $ (mapLeavesId f s :: t)

class Mappable s t a b where
  mapLeavesId :: (forall x . a x -> b x) -> s -> t

instance (Mappable left left' a b, Mappable right right' a b) => Mappable (left :<|> right) (left' :<|> right') a b where
  mapLeavesId f (left :<|> right) = mapLeavesId f left :<|> mapLeavesId f right

instance Mappable s t a b => Mappable (arg -> s) (arg -> t) a b where
  mapLeavesId f s = mapLeavesId f . s

instance Mappable (a x) (b x) a b where
  mapLeavesId f a = f a

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
