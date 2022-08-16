module Main (main) where

import PlutusTx.Prelude
import Prelude (IO)

-- import Plutus.Test.Model (readDefaultBchConfig)
import Test.Tasty (defaultMain, testGroup)

-- import Test.Plutip qualified as Plutip
-- import Test.Quickcheck qualified as Quickcheck
-- import Test.Resources qualified as Resources
import Test.Script.FeeWithdraw qualified as FeeWithdraw
import Test.Script.TokenBurn qualified as TokenBurn
import Test.Script.TokenChangeOwner qualified as TokenChangeOwner
import Test.Script.TokenChangePrice qualified as TokenChangePrice
import Test.Script.TokenMarketplaceBuy qualified as TokenMarketplaceBuy
import Test.Script.TokenMarketplaceRedeem qualified as TokenMarketplaceRedeem
import Test.Script.TokenMarketplaceSetPrice qualified as TokenMarketplaceSetPrice
import Test.Script.TokenMint qualified as TokenMint
import Test.Script.TokenRestake qualified as TokenRestake
import Test.Script.TokenUnstake qualified as TokenUnstake
import Test.Size qualified as Size

main :: IO ()
main = do
  -- cfg <- readDefaultBchConfig
  defaultMain $
    testGroup
      "Seabug Onchain"
      [ Size.test
      , -- , Resources.test cfg
        testGroup
          "Token"
          [ TokenMint.test
          , TokenChangeOwner.test
          , TokenChangePrice.test
          , TokenBurn.test
          ]
      , testGroup
          "Staking"
          [ TokenUnstake.test
          , TokenRestake.test
          ]
      , testGroup
          "Marketplace"
          [ TokenMarketplaceSetPrice.test
          , TokenMarketplaceBuy.test
          , TokenMarketplaceRedeem.test
          ]
      , FeeWithdraw.test
      -- , Quickcheck.test
      -- , Plutip.test
      ]
