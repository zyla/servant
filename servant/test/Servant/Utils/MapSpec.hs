{-# LANGUAGE TypeOperators #-}

module Servant.Utils.MapSpec where

import           Data.Functor.Identity
import           Data.Proxy
import           Test.Hspec

import           Servant.API
import           Servant.Utils.Map

spec :: Spec
spec = do
  describe "mapLeaves" $ do
    it "maps leaves of fish structures" $ do
      let foo :: Bool -> Int -> String
          foo _ b = show b

          bar :: () -> String -> Int -> Double
          bar _ _ i = fromIntegral i

          foobar = foo :<|> bar

          convert :: (Int -> a) -> Identity a
          convert f = Identity $ f 42

          foo' :: Bool -> String
          bar' :: () -> String -> Double

          proxy :: Proxy ((Bool -> Identity String) :<|> (() -> String -> Identity Double))
          proxy = Proxy
          foo' :<|> bar' = mapLeaves convert foobar proxy

      foo' True `shouldBe` "42"
      bar' () "" `shouldBe` (42 :: Double)

  describe "stripIdentity" $ do
    it "works" $ do
      let foo :: Identity String
          foo = Identity "foo"
          bar :: () -> Identity Bool
          bar () = Identity True
          foobar = foo :<|> bar
          foo' :<|> bar' = stripIdentity foobar
      foo' `shouldBe` ("foo" :: String)
      bar' () `shouldBe` True
