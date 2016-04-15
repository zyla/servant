module Servant.CoMock.Internal.Testable where

import Control.Monad.Except
import Test.QuickCheck (Arbitrary(..), discard)
import Test.QuickCheck.Property (Testable(..), forAllShrink, ioProperty, (.&.))
import Servant.Client (ServantError(..))
import Servant.API

import Servant.CoMock.Internal.Predicates

type ServantRes a = ExceptT ServantError IO a


-- | Two corresponding client functions. Used for checking that APIs match.
data ShouldMatch a = ShouldMatch a a

instance (Show a, Eq a) => Testable (ShouldMatch (ServantRes a)) where
    property (ShouldMatch e1 e2) = ioProperty $ do
        e1' <- runExceptT e1
        e2' <- runExceptT e2
        return $ e1' == e2'

instance (Arbitrary a, Show a, Testable (ShouldMatch b))
      => Testable (ShouldMatch (a -> b)) where
    property (ShouldMatch f1 f2) = forAllShrink arbitrary shrink go
      where go x = ShouldMatch (f1 x) (f2 x)

instance (Testable (ShouldMatch a), Testable (ShouldMatch b))
      => Testable (ShouldMatch (a :<|> b)) where
    property (ShouldMatch (a1 :<|> b1) (a2 :<|> b2))
      = property (ShouldMatch a1 a2) .&. property (ShouldMatch b1 b2)


data ShouldSatisfy filter expect a = ShouldSatisfy
  { ssVal :: a
  , ssFilter :: Predicates filter
  , ssExpect :: Predicates expect
  } deriving (Functor)

instance (Show a, Eq a, HasPredicate expect (Either ServantError a))
      => Testable (ShouldSatisfy filter expect (ServantRes a)) where
    property (ShouldSatisfy a _ e) = ioProperty $ do
        a' <- runExceptT a
        return $ getPredicate e a'

instance ( Arbitrary a, Show a, Testable (ShouldSatisfy filter expect b)
         , HasPredicate filter a)
      => Testable (ShouldSatisfy filter expect (a -> b)) where
    property (ShouldSatisfy g f e) = forAllShrink arbitrary shrink go
        where go x | getPredicate f x = ShouldSatisfy (g x) f e
                   | otherwise        = discard

instance ( Testable (ShouldSatisfy filter expect a)
         , Testable (ShouldSatisfy filter expect b))
      => Testable (ShouldSatisfy filter expect (a :<|> b)) where
    property (ShouldSatisfy (a :<|> b) f e)
      = property (ShouldSatisfy a f e) .&. property (ShouldSatisfy b f e)
