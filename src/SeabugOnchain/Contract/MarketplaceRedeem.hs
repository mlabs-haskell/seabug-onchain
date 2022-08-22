module SeabugOnchain.Contract.MarketplaceRedeem (marketplaceRedeem) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Ledger (ChainIndexTxOut (_ciTxOutValue), Redeemer (Redeemer), minAdaTxOut, scriptCurrencySymbol, scriptHashAddress)
import Ledger.Ada (toValue)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (toBuiltinData)
import Plutus.V1.Ledger.Value (assetClass, singleton, valueOf)
import Text.Printf (printf)

import SeabugOnchain.Contract.Aux (getAddrUtxos, getUserUtxos)
import SeabugOnchain.Marketplace
import SeabugOnchain.Token (mkTokenName, policyData)
import SeabugOnchain.Types

-- | Redeem nft from the marketplace. To redeem nft it must be reminted so price is increased by 1 lovelace
marketplaceRedeem :: NftData -> UserContract NftData
marketplaceRedeem nftData = do
  let collection = nftData'nftCollection nftData
      policy' = policyData collection
      curr = scriptCurrencySymbol policy'
      valHash = validatorHash marketplaceValidator
      scriptAddr = scriptHashAddress valHash
      newPrice = toEnum (fromEnum (nftId'price oldNft) + 1)
      oldNft = nftData'nftId nftData
      newNft = oldNft {nftId'price = newPrice}
      oldName = mkTokenName oldNft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangePrice oldNft newPrice
      containsNft (_, tx) = valueOf (_ciTxOutValue tx) curr oldName == 1
  utxo' <- find containsNft . Map.toList <$> getAddrUtxos scriptAddr
  (utxo, utxoIndex) <- case utxo' of
    Nothing -> Contract.throwError "NFT not found on marketplace"
    Just x -> Hask.pure x
  pkh <- Contract.ownFirstPaymentPubKeyHash
  userUtxos <- getUserUtxos
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
          , Constraints.mustPayToPubKey pkh (newNftValue <> toValue minAdaTxOut)
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.tell . Hask.pure $ NftData collection newNft
  Contract.logInfo @Hask.String $ printf "Redeem successful: %s" (Hask.show $ assetClass curr newName)
  Hask.pure $ NftData collection newNft
