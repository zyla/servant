module Servant.CoMock.Internal.CoMock where

import Data.Proxy (Proxy)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Test.QuickCheck (Property, property, stdArgs, Testable, Args(..), quickCheckWithResult, Result(..))
import System.IO.Unsafe (unsafePerformIO)
import Servant.Client (HasClient, BaseUrl, client, Client)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (withApplication)
import Test.Hspec (Expectation, shouldSatisfy, expectationFailure)
import Servant.Client (BaseUrl(..), Scheme(..))
import Servant (serveWithContext, Server, HasServer, Context, serve)
import Network.HTTP.Client (Request, managerModifyRequest)

import Servant.CoMock.Internal.Testable
import Servant.CoMock.Internal.Predicates

-- | Start a servant application on an open port, run the provided function,
-- then stop the application.
withServantServer :: HasServer a '[] => Proxy a -> Server a -> (BaseUrl -> IO r) -> IO r
withServantServer api server t
  = withApplication (return $ serve api server) $ \port ->
      t (BaseUrl Http "localhost" port "")

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
-- functions such as 'quickCheckWith'. For most use cases, you should use
-- @serversEqual@ or @servantServersEqual@.
serversEqualProperty :: (HasClient a, Testable (ShouldMatch (Client a)))
    => Proxy a -> Manager -> BaseUrl -> BaseUrl -> Property
serversEqualProperty api mgr burl1 burl2 = property $ ShouldMatch c1 c2
  where c1 = client api burl1 mgr
        c2 = client api burl2 mgr

-- | Check that the two servers at the provided @BaseUrl@s behave identically.
--
-- As with @serversEqualProperty@, non-determinism in the servers will likely
-- result in failures that may not be significant.
serversEqual :: (HasClient a, Testable (ShouldMatch (Client a)))
    => Proxy a -> BaseUrl -> BaseUrl -> Int -> Expectation
serversEqual api burl1 burl2 tries = do
    mgr <- newManager defaultManagerSettings
    let args = stdArgs { chatty = False, maxSuccess = tries }
    res <- quickCheckWithResult args $ serversEqualProperty api mgr burl1 burl2
    res `shouldSatisfy` isSuccess


serverSatisfiesProperty :: (HasClient a, Testable (ShouldSatisfy filt exp (Client a)))
    => Proxy a -> Manager -> BaseUrl -> Predicates filt -> Predicates exp -> Property
serverSatisfiesProperty api mgr burl filters expect = do
    property $ ShouldSatisfy (client api burl mgr) filters expect

serverSatisfies :: (HasClient a, Testable (ShouldSatisfy filt exp (Client a)))
    => Proxy a -> BaseUrl -> Predicates filt -> Predicates exp
    -> Int -> Expectation
serverSatisfies api burl filters expect tries = do
    mgr <- managerWithStoredReq
    let args = stdArgs { chatty = False, maxSuccess = tries }
    res <- quickCheckWithResult args $ serverSatisfiesProperty api mgr burl filters expect
    if isSuccess res
      then return ()
      else do
        Just r <- readMVar currentReq
        expectationFailure $ show r ++ "\nResponse:\n" ++ show res

isSuccess (Success _ _ _) = True
isSuccess _               = False

-- | Check that the two servers at the provided @BaseUrl@s do not behave identically.
--
-- As with @serversEqualProperty@, non-determinism in the servers will likely
-- result in failures that may not be significant.
serversUnequal :: (HasClient a, Testable (ShouldMatch (Client a)))
    => Proxy a -> BaseUrl -> BaseUrl -> Int -> Expectation
serversUnequal api burl1 burl2 tries = do
    mgr <- newManager defaultManagerSettings
    let args = stdArgs { chatty = False, maxSuccess = tries }
    res <- quickCheckWithResult args $ serversEqualProperty api mgr burl1 burl2
    res `shouldSatisfy` not . isSuccess

serverDoesntSatisfy :: (HasClient a, Testable (ShouldSatisfy filt exp (Client a)))
    => Proxy a -> BaseUrl -> Predicates filt -> Predicates exp
    -> Int -> Expectation
serverDoesntSatisfy api burl filters expect tries = do
    mgr <- newManager defaultManagerSettings
    let args = stdArgs { chatty = False, maxSuccess = tries }
    res <- quickCheckWithResult args $ serverSatisfiesProperty api mgr burl filters expect
    res `shouldSatisfy` not . isSuccess

-- Used to store the current request being made so that in case of failure we
-- have the failing test in a user-friendly form.
currentReq :: MVar (Maybe Request)
currentReq = unsafePerformIO $ newMVar Nothing
{-# NOINLINE currentReq #-}

managerWithStoredReq :: IO Manager
managerWithStoredReq = newManager defaultManagerSettings { managerModifyRequest = go }
  where go req = modifyMVar_ currentReq (const $ return $ Just req) >> return req
