{-# LANGUAGE TypeOperators #-}

module Servant.Utils.MapSpec where

import           Test.Hspec

import           Servant.API
import           Servant.Utils.Map

spec :: Spec
spec = do
  describe "supplyArgument" $ do
{-    it "supplies an argument to every route" $ do
      let foo :: Bool -> Int -> String
          foo _ b = show b

          bar :: () -> String -> Int -> Double
          bar _ _ i = fromIntegral i

          foobar = foo :<|> bar

          foo' :: Bool -> String
          bar' :: () -> String -> Double
          foo' :<|> bar' = supplyArgument (42 :: Int) foobar

      foo' True `shouldBe` "42"
      bar' () "" `shouldBe` (42 :: Double)
-}
    it "" $ do
      let foo :: Int -> String
          foo = show
          bar :: Int -> String
          bar = show . succ

          without :: String
          bar' :: String
          without :<|> bar' = supplyArgument (42 :: Int) (foo :<|> bar)
      without `shouldBe` "42"
      bar' `shouldBe` "43"
