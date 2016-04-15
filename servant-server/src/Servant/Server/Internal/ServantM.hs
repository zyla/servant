{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Servant.Server.Internal.ServantM where

import GHC.Generics (Generic)
import Control.Monad (ap, liftM)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.State (StateT(..))

import Servant.Server.Internal.ServantErr

newtype ServantM errs a = ServantM
 { runServantT' :: ExceptT ServantErr a }
 deriving (Generic)

instance (n `Elem` errs) => HttpError n (ServantM errs) where
  throwHttpError =

{-runServantT :: ServantM s a -> s -> IO (Either ServantErr a, s)-}
{-runServantT m s = runStateT s  . runExceptT $ runServantT' m-}

{-evalServantT :: ServantM s a -> s -> IO (Either ServantErr a)-}
{-evalServantT m s = liftM fst (runServantT m s)-}

instance Functor (ServantM s) where
  fmap f = ServantM . fmap f . runServantT'

instance Applicative (ServantM s) where
  pure = return
  (<*>) = ap

instance Monad (ServantM s) where
  return = ServantM . return
  {-ma >>= f = runServantT' ma >>=-}

instance MonadError ServantErr (ServantM s) where
  throwError e = ServantM $ throwError e
  catchError m c = ServantM $ runServantT' m `catchError` c

{-instance MonadWriter s (ServantM s) where-}
  {-writer = ServantM . writer-}
  {-listen = ServantM . listen . runServantT'-}

{-instance MonadReader s (ServantM s) where-}
