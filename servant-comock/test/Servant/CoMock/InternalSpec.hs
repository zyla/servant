module Servant.CoMock.InternalSpec (spec) where

import Control.Concurrent (forkIO, killThread)
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client
import Test.Hspec
import Test.QuickCheck

import Servant.CoMock.Internal
import Servant.CoMock

spec :: Spec
spec = do
  serversEqualSpec


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

{-
type StatefulAPI = ReqBody '[JSON] String :> Post '[JSON] Int
              :<|> Get '[JSON] String

statefulMVar :: MVar String
statefulMVar = unsafePerformIO $ newMVar ""

statefulAPI :: Proxy StatefulAPI
statefulAPI = Proxy

statefulAPIServer :: Server StatefulAPI
statefulAPIServer = (\x -> liftIO $ swapMVar statefulMVar x)
               :<|> liftIO $ readMVar statefulMVar >>= return . length
-}

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

-- Ports 5381 and 5382 must be usable
testPort1, testPort2 :: Int
testPort1 = 5381
testPort2 = 5382


testServers :: (Testable (ShouldMatch (Client a)), HasServer a, HasClient a) => Proxy a -> Server a -> Server a -> IO Result
testServers api s1 s2 = do
    mgr <- newManager defaultManagerSettings
    t1 <- forkIO $ run testPort1 $ serve api s1
    t2 <- forkIO $ run testPort2 $ serve api s2
    print "here"
    res <- serversEqual api mgr (BaseUrl Http "localhost" testPort1 "")
                                (BaseUrl Http "localhost" testPort2 "")
                                stdArgs
    killThread t1
    killThread t2
    return res

testServersEq :: (Testable (ShouldMatch (Client a)), HasServer a, HasClient a)
    => Proxy a -> Server a -> Expectation
testServersEq api s1 = do
    r <- testServers api s1 s1
    r `shouldSatisfy` isSuccess

testServersUneq :: (Testable (ShouldMatch (Client a)), HasServer a, HasClient a)
    => Proxy a -> Server a -> Server a -> Expectation
testServersUneq api s1 s2 = do
    r <- testServers api s1 s2
    r `shouldSatisfy` not . isSuccess

isSuccess :: Result -> Bool
isSuccess (Success _ _ _) = True
isSuccess _               = False
