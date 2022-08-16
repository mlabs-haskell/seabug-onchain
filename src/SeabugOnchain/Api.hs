module SeabugOnchain.Api (
  ApiUserContract,
  endpoints,
  NFTAppSchema,
) where

import Control.Monad (forever, void)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Plutus.Contract (Contract, Promise, endpoint, selectList)
import PlutusTx.Prelude

import SeabugOnchain.Contract.Burn (burn)
import SeabugOnchain.Contract.ChangeOwner (changeOwner)
import SeabugOnchain.Contract.FeeWithdraw (feeWithdraw)
import SeabugOnchain.Contract.MarketplaceBuy (marketplaceBuy)
import SeabugOnchain.Contract.MarketplaceDeposit (marketplaceDeposit)
import SeabugOnchain.Contract.MarketplaceRedeem (marketplaceRedeem)
import SeabugOnchain.Contract.MarketplaceSetPrice (marketplaceSetPrice)
import SeabugOnchain.Contract.Mint (mint, mintWithCollection)
import SeabugOnchain.Contract.SetPrice (setPrice)
import SeabugOnchain.Types (NFTAppSchema, NftData)

type ApiUserContract a = Contract (Last NftData) NFTAppSchema Text a

-- | User Endpoints .
endpoints :: ApiUserContract ()
endpoints = forever $ selectList tokenEndpointsList

-- | List of User Promises.
tokenEndpointsList :: [Promise (Last NftData) NFTAppSchema Text ()]
tokenEndpointsList =
  [ void $ endpoint @"mint" mint
  , void $ endpoint @"mint-with-collection" mintWithCollection
  , endpoint @"change-owner" changeOwner
  , void $ endpoint @"set-price" setPrice
  , void $ endpoint @"marketplace-deposit" marketplaceDeposit
  , void $ endpoint @"marketplace-redeem" marketplaceRedeem
  , void $ endpoint @"marketplace-buy" marketplaceBuy
  , void $ endpoint @"marketplace-set-price" marketplaceSetPrice
  , endpoint @"burn" burn
  , endpoint @"fee-withdraw" feeWithdraw
  ]
