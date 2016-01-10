{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Servant.Server.Internal.ConfigSpec (spec) where

import           Test.Hspec                     (Spec, describe, it, shouldBe, context)
import           Test.ShouldNotTypecheck        (shouldNotTypecheck)
import           Control.DeepSeq                (NFData)


import           Servant.API
import           Servant.Server.Internal.Config

newtype WrappedInt = WrappedInt Int deriving (Eq, Show, NFData)
newtype WrappedChar = WrappedChar Char deriving (Eq, Show, NFData)

spec :: Spec
spec = getConfigEntrySpec

getConfigEntrySpec :: Spec
getConfigEntrySpec = describe "getConfigEntry" $ do

  let cfg1 = WrappedInt 0 .:. EmptyConfig :: Config '[WrappedInt]
      cfg2 = 1 .:. cfg1 :: Config '[Int, WrappedInt]

  it "gets the config if a matching one exists" $
    getConfigEntry cfg1 `shouldBe` WrappedInt 0

  it "gets the first matching config" $
    getConfigEntry cfg2 `shouldBe` 1

  it "allows to distinguish between different config entries with the same type by tag" $ do
    let cfg = 'a' .:. WrappedChar 'b' .:. EmptyConfig :: Config '[Char, WrappedChar]
    (getConfigEntry cfg :: Char) `shouldBe` 'a'

  context "Show instance" $ do
    let cfg = (1 :: Integer) .:. (2 :: Integer) .:. EmptyConfig
    it "has a Show instance" $
      show cfg `shouldBe` "1 .:. 2 .:. EmptyConfig"

    it "bracketing works" $
      show (Just cfg) `shouldBe` "Just (1 .:. 2 .:. EmptyConfig)"

    it "bracketing works with operators" $ do
      let cfg' = ((1 :: Integer) .:. 'a' .:. EmptyConfig) :<|> ('b' .:. True .:. EmptyConfig)
      show cfg' `shouldBe` "(1 .:. 'a' .:. EmptyConfig) :<|> ('b' .:. True .:. EmptyConfig)"

  it "does not typecheck if type does not exist in config" $ do
    let x = getConfigEntry cfg1 :: Char
    shouldNotTypecheck x

  -- it "does not typecheck if key maps to a different type" $ do
    -- let x = getConfigEntry  cfg1 :: String
    -- shouldNotTypecheck x
