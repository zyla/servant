module Servant.CoMock where

import Data.Proxy (Proxy)
import Test.QuickCheck (Property)
import Servant.Client (HasClient, BaseUrl)

import Servant.CoMock.Internal

-- | A QuickCheck 'Property' that randomly generates arguments (captures, query
-- params, request bodies, headers, etc.) expected by endpoints of a server,
-- and makes requests to the servers running in the two provided URLs in the
-- same order, failing if they do not return the same response.
--
-- Evidently, if the behaviour of the server is expected to be
-- non-deterministic, or if depends fundamentally on time, this function may
-- produce spurious failures.
serversEqual :: HasClient a => Proxy a -> BaseUrl -> BaseUrl -> IO Property
serversEqual = undefined
