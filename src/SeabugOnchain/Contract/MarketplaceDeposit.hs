module SeabugOnchain.Contract.MarketplaceDeposit (marketplaceDeposit) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Ledger (Datum (Datum), minAdaTxOut, scriptCurrencySymbol)
import Ledger.Ada (lovelaceValueOf, toValue)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton)
import Text.Printf (printf)

import SeabugOnchain.Contract.Aux
import SeabugOnchain.Marketplace
import SeabugOnchain.Token (mkTokenName, policyData)
import SeabugOnchain.Types

-- | Deposit nft in the marketplace
marketplaceDeposit :: NftData -> UserContract NftData
marketplaceDeposit nftData = do
  let policy' = policyData . nftData'nftCollection $ nftData
      curr = scriptCurrencySymbol policy'
      tn = mkTokenName . nftData'nftId $ nftData
      nftValue = singleton curr tn 1
      valHash = validatorHash marketplaceValidator
  pkh <- Contract.ownFirstPaymentPubKeyHash
  utxos <- getUserUtxos
  let lookup =
        Hask.mconcat
          [ Constraints.plutusV1MintingPolicy policy'
          , Constraints.unspentOutputs utxos
          , Constraints.plutusV1TypedValidatorLookups marketplaceValidator
          , Constraints.plutusV1OtherScript (validatorScript marketplaceValidator)
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustPayToOtherScript
              valHash
              (Datum . toBuiltinData . MarketplaceDatum $ assetClass curr tn)
              (nftValue <> toValue minAdaTxOut)
          , Constraints.mustPayToPubKey pkh $ lovelaceValueOf 5_000_000
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ nftData
  Contract.logInfo @Hask.String $ printf "Deposit successful: %s" (Hask.show $ assetClass curr tn)
  Hask.pure nftData
