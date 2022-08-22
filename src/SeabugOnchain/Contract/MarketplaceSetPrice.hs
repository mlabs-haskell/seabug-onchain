module SeabugOnchain.Contract.MarketplaceSetPrice (marketplaceSetPrice) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Ledger (Datum (Datum), Redeemer (Redeemer), minAdaTxOut, scriptCurrencySymbol, scriptHashAddress, _ciTxOutValue)
import Ledger.Ada (toValue)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton, valueOf)
import Text.Printf (printf)

import SeabugOnchain.Contract.Aux
import SeabugOnchain.Marketplace
import SeabugOnchain.Token
import SeabugOnchain.Types

marketplaceSetPrice :: SetPriceParams -> UserContract NftData
marketplaceSetPrice sp = do
  let collection = nftData'nftCollection . sp'nftData $ sp
      policy' = policyData collection
      curr = scriptCurrencySymbol policy'
      valHash = validatorHash marketplaceValidator
      scriptAddr = scriptHashAddress valHash
      oldNft = nftData'nftId . sp'nftData $ sp
      newNft = oldNft {nftId'price = sp'price sp}
      oldName = mkTokenName oldNft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangePrice oldNft (sp'price sp)
      containsNft (_, tx) = valueOf (_ciTxOutValue tx) curr oldName == 1
      nftData = NftData collection newNft
  utxo' <- find containsNft . Map.toList <$> getAddrUtxos scriptAddr
  (utxo, utxoIndex) <- case utxo' of
    Nothing -> Contract.throwError "NFT not found on marketplace"
    Just x -> Hask.pure x
  pkh <- Contract.ownFirstPaymentPubKeyHash
  userUtxos <- getUserUtxos
  Contract.logInfo @Hask.String $ printf "Script UTXOs: %s" (Hask.show . _ciTxOutValue $ utxoIndex)
  let lookup =
        Hask.mconcat
          [ Constraints.plutusV1MintingPolicy policy'
          , Constraints.plutusV1TypedValidatorLookups marketplaceValidator
          , Constraints.plutusV1OtherScript (validatorScript marketplaceValidator)
          , Constraints.unspentOutputs $ Map.insert utxo utxoIndex userUtxos
          , Constraints.ownPaymentPubKeyHash pkh
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustBeSignedBy pkh
          , Constraints.mustSpendScriptOutput utxo (Redeemer $ toBuiltinData ())
          , Constraints.mustPayToOtherScript
              valHash
              (Datum . toBuiltinData . MarketplaceDatum $ assetClass curr newName)
              (newNftValue <> toValue minAdaTxOut)
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ nftData
  Contract.logInfo @Hask.String $ printf "Marketplace set price successful: %s" (Hask.show $ assetClass curr newName)
  Hask.pure nftData
