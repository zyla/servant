module Servant.CoMock where

import Data.Proxy (Proxy)
import Test.QuickCheck (Property, property, Testable, Args, quickCheckWithResult, Result)
import Servant.Client (HasClient, BaseUrl, client, Client)
import Network.HTTP.Client (Manager(..))

import Servant.CoMock.Internal

-- | A QuickCheck 'Property' that randomly generates arguments (captures, query
-- params, request bodies, headers, etc.) expected by endpoints of a server,
-- and makes requests to the servers running in the two provided URLs in the
-- same order, failing if they do not return the same response.
--
-- Evidently, if the behaviour of the server is expected to be
-- non-deterministic,  this function may produce spurious failures.
--
-- Note that this QuickCheck 'Property' does IO; interleaving it with other IO
-- actions will not work. It is provided so that it can be used with QuickCheck
-- functions such as 'quickCheckWith'.
serversEqualProperty :: (HasClient a, Testable (ShouldMatch (Client a)))
    => Proxy a -> Manager -> BaseUrl -> BaseUrl -> Property
serversEqualProperty api mgr burl1 burl2 = property $ ShouldMatch c1 c2
  where c1 = client api burl1 mgr
        c2 = client api burl2 mgr

serversEqual :: (HasClient a, Testable (ShouldMatch (Client a)))
    => Proxy a -> Manager -> BaseUrl -> BaseUrl -> Args -> IO Result
serversEqual api mgr burl1 burl2 args = quickCheckWithResult args
    $ serversEqualProperty api mgr burl1 burl2
