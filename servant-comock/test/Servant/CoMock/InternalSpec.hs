module Servant.CoMock.InternalSpec (spec) where

import Control.Concurrent (forkIO, killThread)
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
import Test.Hspec
import Test.QuickCheck

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

testServers :: Proxy a -> Server a -> Server a -> Property
testServers p s1 s2 = ioProperty $ do
    t1 <- forkIO $ run testPort1 $ serve p s1
    t2 <- forkIO $ run testPort2 $ serve p s2
    p <- serversEqual p (BaseUrl Http "localhost" testPort1)
                        (BaseUrl Http "localhost" testPort1)
    killThread t1
    killThread t2
    return p

testServersEq :: Proxy a -> Server a -> Property
testServersEq p s1 = testServers p s1 s1

testServersUneq :: Proxy a -> Server a -> Server a -> Property
testServersUneq p s1 s2 = expectFailure $ testServers s1 s2
