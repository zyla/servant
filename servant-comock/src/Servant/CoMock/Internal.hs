module Servant.CoMock.Internal where

import Control.Monad.Except
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Property (Testable(..), forAllShrink, ioProperty, (.&.))
import Servant.Client (ServantError(..))
import Servant.API

import Debug.Trace

-- | Two corresponding client functions.
data ShouldMatch a = ShouldMatch a a

type ServantRes a = ExceptT ServantError IO a

-- TODO: Probably best fixed in servant-client
instance Eq ServantError where
    e1 == e2 = True

instance (Show a, Eq a) => Testable (ShouldMatch (ServantRes a)) where
    property (ShouldMatch e1 e2) = ioProperty $ do
        e1' <- runExceptT e1
        e2' <- runExceptT e2
        return $ traceShow (e1', e2') $ e1' == e2'

instance (Arbitrary a, Show a, Testable (ShouldMatch b))
      => Testable (ShouldMatch (a -> b)) where
    property (ShouldMatch f1 f2) = forAllShrink arbitrary shrink go
      where go x = ShouldMatch (f1 x) (f2 x)

instance (Testable (ShouldMatch a), Testable (ShouldMatch b))
      => Testable (ShouldMatch (a :<|> b)) where
    property (ShouldMatch (a1 :<|> b1) (a2 :<|> b2))
      = property (ShouldMatch a1 a2) .&. property (ShouldMatch b1 b2)
