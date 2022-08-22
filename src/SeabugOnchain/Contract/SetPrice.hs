module SeabugOnchain.Contract.SetPrice (setPrice) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Void (Void)
import Ledger (Redeemer (Redeemer), minAdaTxOut, scriptCurrencySymbol)
import Ledger.Ada (toValue)
import Ledger.Constraints qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton)
import Text.Printf (printf)

import SeabugOnchain.Contract.Aux (getUserUtxos)
import SeabugOnchain.Token
import SeabugOnchain.Types

setPrice :: SetPriceParams -> UserContract NftData
setPrice sp = do
  pkh <- Contract.ownFirstPaymentPubKeyHash
  utxos <- getUserUtxos
  let collection = nftData'nftCollection . sp'nftData $ sp
      policy' = policyData collection
      curr = scriptCurrencySymbol policy'
      oldNft = nftData'nftId . sp'nftData $ sp
      newNft = oldNft {nftId'price = sp'price sp}
      oldName = mkTokenName oldNft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      mintRedeemer = Redeemer . toBuiltinData $ ChangePrice oldNft (sp'price sp)
      nftData = NftData collection newNft
      lookup =
        Hask.mconcat
          [ Constraints.plutusV1MintingPolicy policy'
          , Constraints.unspentOutputs utxos
          , Constraints.ownPaymentPubKeyHash pkh
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustPayToPubKey pkh (newNftValue <> toValue minAdaTxOut)
          , Constraints.mustBeSignedBy pkh
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ nftData
  Contract.logInfo @Hask.String $ printf "Set price successful: %s" (Hask.show $ assetClass curr newName)
  Hask.pure nftData
