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

------------------------------------------------------------------------------
-- APIs
------------------------------------------------------------------------------

type OnlyReturnAPI = Get '[JSON] Int
                :<|> Post '[JSON] String

onlyReturnAPI :: Proxy OnlyReturnAPI
onlyReturnAPI = Proxy

onlyReturnAPIServer :: Server OnlyReturnAPI
onlyReturnAPIServer = return 5 :<|> return "hi"

onlyReturnAPIServer' :: Server OnlyReturnAPI
onlyReturnAPIServer' = return 5 :<|> return "hia"

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

-- Ports 5381 and 5382 must be usable
testPort1, testPort2 :: Int
testPort1 = 5381
testPort2 = 5382


testServers :: (Testable (ShouldMatch (Client a)), HasServer a, HasClient a) => Proxy a -> Server a -> Server a -> Property
testServers api s1 s2 = ioProperty $ do
    mgr <- newManager defaultManagerSettings
    t1 <- forkIO $ run testPort1 $ serve api s1
    t2 <- forkIO $ run testPort2 $ serve api s2
    let p = serversEqual api mgr (BaseUrl Http "localhost" testPort1 "")
                        (BaseUrl Http "localhost" testPort1 "")
    killThread t1
    killThread t2
    return p

testServersEq :: (Testable (ShouldMatch (Client a)), HasServer a, HasClient a)
    => Proxy a -> Server a -> Property
testServersEq api s1 = testServers api s1 s1

testServersUneq :: (Testable (ShouldMatch (Client a)), HasServer a, HasClient a)
    => Proxy a -> Server a -> Server a -> Property
testServersUneq api s1 s2 = expectFailure $ testServers api s1 s2
