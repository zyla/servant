{-# LANGUAGE CPP #-}
module Servant.CoMock.InternalSpec (spec) where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Servant
import Servant.Client
import Test.Hspec
import Test.QuickCheck
import System.IO.Unsafe (unsafePerformIO)

import Servant.CoMock.Internal

spec :: Spec
spec = do
  serversEqualSpec
  serverSatisfiesSpec


serversEqualSpec :: Spec
serversEqualSpec = describe "serversEqual" $ do

  context "servers without function types" $ do

    it "considers equal servers equal" $ do
      testServersEq onlyReturnAPI onlyReturnAPIServer

    it "considers unequal servers unequal" $ do
      testServersUneq onlyReturnAPI onlyReturnAPIServer onlyReturnAPIServer'

  context "servers with function types" $ do

    it "considers equal servers equal" $ do
      testServersEq functionAPI functionAPIServer

    it "considers unequal servers unequal" $ do
      testServersUneq functionAPI functionAPIServer functionAPIServer'

  context "stateful servers" $ do

    it "considers equal servers equal" $ do
      withServantServer statefulAPI statefulAPIServer1 $ \burl1 ->
        withServantServer statefulAPI statefulAPIServer2 $ \burl2 ->
           serversEqual statefulAPI burl1 burl2 noOfTests

serverSatisfiesSpec :: Spec
serverSatisfiesSpec = describe "serverSatisfies" $ do

  it "passes true predicates" $ do
    let e = addRightPredicate (== (4 :: Int)) emptyPredicates
    withServantServer onlyReturnAPI onlyReturnAPIServer $ \burl ->
      serverSatisfies onlyReturnAPI burl emptyPredicates e noOfTests

  it "fails false predicates" $ do
    let e = addRightPredicate (== (4 :: Int)) emptyPredicates
    withServantServer onlyReturnAPI onlyReturnAPIServer $ \burl ->
      serverDoesntSatisfy onlyReturnAPI burl emptyPredicates e noOfTests

  context "never500s" $ do

    it "is true for servers that don't return 500s" $ do
      withServantServer functionAPI functionAPIServer $ \burl ->
        serverSatisfies functionAPI burl emptyPredicates never500s noOfTests

    it "is false for servers that return 500s" $ do
      withServantServer onlyReturnAPI onlyReturnAPIServer'' $ \burl ->
        serverDoesntSatisfy onlyReturnAPI burl emptyPredicates never500s noOfTests

------------------------------------------------------------------------------
-- APIs
------------------------------------------------------------------------------

-- * OnlyReturn

type OnlyReturnAPI = Get '[JSON] Int
                :<|> Post '[JSON] String

onlyReturnAPI :: Proxy OnlyReturnAPI
onlyReturnAPI = Proxy

onlyReturnAPIServer :: Server OnlyReturnAPI
onlyReturnAPIServer = return 5 :<|> return "hi"

onlyReturnAPIServer' :: Server OnlyReturnAPI
onlyReturnAPIServer' = return 5 :<|> return "hia"

onlyReturnAPIServer'' :: Server OnlyReturnAPI
onlyReturnAPIServer'' = error "err" :<|> return "hia"

-- * Function

type FunctionAPI = ReqBody '[JSON] String :> Post '[JSON] Int
              :<|> Header "X-abool" Bool :> Get '[JSON] (Maybe Bool)

functionAPI :: Proxy FunctionAPI
functionAPI = Proxy

functionAPIServer :: Server FunctionAPI
functionAPIServer = return . length :<|> return

functionAPIServer' :: Server FunctionAPI
functionAPIServer' = (\x -> return $ length x - 1) :<|> \x -> return (not <$> x)

-- * Stateful

type StatefulAPI = ReqBody '[JSON] String :> Post '[JSON] String
              :<|> Get '[JSON] Int

statefulMVar1 :: MVar String
statefulMVar1 = unsafePerformIO $ newMVar ""
{-# NOINLINE statefulMVar1 #-}

statefulMVar2 :: MVar String
statefulMVar2 = unsafePerformIO $ newMVar ""
{-# NOINLINE statefulMVar2 #-}

statefulAPI :: Proxy StatefulAPI
statefulAPI = Proxy

statefulAPIServer1 :: Server StatefulAPI
statefulAPIServer1 = (\x -> liftIO $ swapMVar statefulMVar1 x)
               :<|> (liftIO $ readMVar statefulMVar1 >>= return . length)

statefulAPIServer2 :: Server StatefulAPI
statefulAPIServer2 = (\x -> liftIO $ swapMVar statefulMVar2 x)
               :<|> (liftIO $ readMVar statefulMVar2 >>= return . length)

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------


testServersEq :: (Testable (ShouldMatch (Client a)), HasServer a '[], HasClient a)
    => Proxy a -> Server a -> Expectation
testServersEq api s1 = withServantServer api s1 $ \burl ->
    serversEqual api burl burl noOfTests

testServersUneq :: (Testable (ShouldMatch (Client a)), HasServer a '[], HasClient a)
    => Proxy a -> Server a -> Server a -> Expectation
testServersUneq api s1 s2 = withServantServer api s1 $ \burl1 ->
      withServantServer api s2 $ \burl2 -> serversUnequal api burl1 burl2 noOfTests


noOfTests :: Int
#if LONG_TESTS
noOfTests = 20000
#else
noOfTests = 500
#endif
