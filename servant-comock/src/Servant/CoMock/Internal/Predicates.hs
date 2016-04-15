{-# LANGUAGE UndecidableInstances #-}
module Servant.CoMock.Internal.Predicates where

import Network.HTTP.Types (statusCode)
import Servant.Common.Req (ServantError(..))
import Data.Void

data Predicates a where
    HNil :: Predicates '[]
    HCons :: (a -> Bool) -> Predicates b -> Predicates (a ': b)

class HasPredicate a b where
    getPredicate :: Predicates a -> b -> Bool

instance {-# OVERLAPPING #-} HasPredicate '[] a where
    getPredicate _ = const True

instance {-# OVERLAPPING #-} HasPredicate (a ': xs) a where
    getPredicate (HCons a _) = a

-- This is a little bit of a hack. Ideally instances would match when the
-- predicate is polymorphic, but that doesn't work since the polymorphic type
-- may have to unify with multiple distict values.
instance {-# OVERLAPPING #-} HasPredicate (Either ServantError Void ': xs)
                                          (Either ServantError a) where
    getPredicate (HCons f _) x = case x of
                                    Left e -> f (Left e)
                                    Right x -> True

instance {-# OVERLAPPABLE #-} ( ls ~ (b ': xs), HasPredicate xs a)
    => HasPredicate ls a where
    getPredicate (HCons _ xs) = getPredicate xs

addPredicate :: (a -> Bool) -> Predicates b -> Predicates (a ': b)
addPredicate = HCons

addRightPredicate :: (a -> Bool) -> Predicates b -> Predicates (Either ServantError a ': b)
addRightPredicate p = addPredicate $ either (const True) p

addLeftPredicate :: (ServantError -> Bool) -> Predicates b
    -> Predicates (Either ServantError a ': b)
addLeftPredicate p = addPredicate $ either p (const True)

emptyPredicates :: Predicates '[]
emptyPredicates = HNil

-- * Useful predicates

never500s :: Predicates '[Either ServantError Void]
never500s = addLeftPredicate go emptyPredicates
  where
   go (FailureResponse x _ _) = statusCode x /= 500
   go _ = True
