module Servant.CoMock.Internal where

import Control.Monad.Except
import Test.QuickCheck.Property (Testable(..), ioProperty, (.&.))
import Servant.Client (ServantError(..))
import Servant.API

-- | Two corresponding client functions.
data ShouldMatch a = ShouldMatch a a

type ServantRes a = ExceptT ServantError IO a

-- TODO: Probably best fixed in servant-client
instance Eq ServantError where
    e1 == e2 = True

instance (Eq a) => Testable (ShouldMatch (ServantRes a)) where
    property (ShouldMatch e1 e2) = ioProperty $ do
        e1' <- runExceptT e1
        e2' <- runExceptT e2
        return $ e1' == e2'

instance (Testable a, Testable b) => Testable (a :<|> b) where
    property (a :<|> b) = property a .&. property b
