-- | This module contains benchmark-related logic.
--
-- Currently it generates 'wrk' scripts rather than benchmarking directly with
-- the @servant-client@ functions since the performance of 'wrk' is
-- significantly better.
module Servant.CoMock.Internal.Benchmarking where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Client
import Network.HTTP.Types
import Servant.Client

data BenchOptions = BenchOptions
  { duration    :: Int
  , threads     :: Int
  , connections :: Int
  , noOfTests   :: Int
  } deriving (Eq, Show, Read)

defaultBenchOptions :: BenchOptions
defaultBenchOptions = BenchOptions
  { duration    = 3
  , threads     = 2
  , connections = 20
  , noOfTests   = 20
  }

data WrkScript = WrkScript
  { wrkScheme  :: Scheme
  , wrkHost    :: ByteString
  , wrkPort    :: Int
  , wrkMethod  :: Method
  , wrkPath    :: ByteString
  , wrkHeaders :: [Header]
  , wrkBody    :: ByteString
  } deriving (Eq, Show)

mkScript :: WrkScript -> String
mkScript w
  =  "wrk.scheme = \"" ++ sscheme (wrkScheme w) ++ "\""
  ++ "\nwrk.host = " ++ show (wrkHost w)
  ++ "\nwrk.port = " ++ show (wrkPort w)
  ++ "\nwrk.method = " ++ show (wrkMethod w)
  ++ "\nwrk.path = " ++ show (wrkPath w)
  ++ foldr (\(h,v) old -> old ++ "\nwrk.headers[" ++ show h ++ "] = " ++ show v)
           ""
           (wrkHeaders w)
  ++ "\nwrk.body = " ++ show (wrkBody w)
  where
    sscheme Http  = "http"
    sscheme Https = "https"

reqToWrk :: Request -> WrkScript
reqToWrk r = WrkScript
  { wrkScheme  = Http
  , wrkHost    = host r
  , wrkPort    = port r
  , wrkMethod  = method r
  , wrkPath    = path r
  , wrkHeaders = requestHeaders r
  , wrkBody    = case requestBody r of
     RequestBodyLBS r' -> toStrict r'
     _ -> error "expecting RequestBodyLBS"
  }
