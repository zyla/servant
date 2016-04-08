{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Servant.Utils.Map where

import           Servant.API

class HasArgument arg s t | arg s -> t where
  supplyArgument :: arg -> s -> t

instance (HasArgument arg left left', HasArgument arg right right') =>
  HasArgument arg (left :<|> right) (left' :<|> right') where

  supplyArgument arg (left :<|> right) =
    supplyArgument arg left :<|> supplyArgument arg right

instance HasArgument arg rest rest' =>
  HasArgument arg (a -> rest) (a -> rest') where

  supplyArgument arg f = \ a -> supplyArgument arg (f a)

instance HasArgument arg (arg -> result) result where
  supplyArgument arg f = f arg
