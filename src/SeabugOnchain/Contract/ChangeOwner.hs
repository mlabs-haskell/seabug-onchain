module SeabugOnchain.Contract.ChangeOwner (changeOwner) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Void (Void)
import Ledger (Datum (Datum), Redeemer (Redeemer), scriptCurrencySymbol)
import Ledger.Constraints qualified as Constraints
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))
import Plutus.V1.Ledger.Value (assetClass, singleton)
import Text.Printf (printf)

import SeabugOnchain.Contract.Aux
import SeabugOnchain.Token
import SeabugOnchain.Types

changeOwner :: ChangeOwnerParams -> UserContract ()
changeOwner cp = do
  utxos <- getUserUtxos
  let collection = nftData'nftCollection . cp'nftData $ cp
      policy' = policyData collection
      curr = scriptCurrencySymbol policy'
      oldNft = nftData'nftId . cp'nftData $ cp
      newNft = oldNft {nftId'owner = cp'owner cp}
      oldName = mkTokenName oldNft
      newName = mkTokenName newNft
      oldNftValue = singleton curr oldName (-1)
      newNftValue = singleton curr newName 1
      nftPrice = fromEnum $ nftId'price oldNft
      mintRedeemer = Redeemer . toBuiltinData $ ChangeOwner oldNft (cp'owner cp)
      getShare share = lovelaceValueOf $ (nftPrice * 10000) `divide` share
      authorShare = getShare (fromEnum $ nftCollection'authorShare collection)
      daoShare = getShare (fromEnum $ nftCollection'daoShare collection)
      ownerShare = lovelaceValueOf nftPrice - authorShare - daoShare
      datum = Datum . PlutusTx.toBuiltinData $ (curr, oldName)
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.unspentOutputs utxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , Constraints.mustPayToPubKey (cp'owner cp) newNftValue
          , Constraints.mustPayWithDatumToPubKey (nftCollection'author collection) datum authorShare
          , Constraints.mustPayWithDatumToPubKey (nftId'owner oldNft) datum ownerShare
          , Constraints.mustPayToOtherScript (nftCollection'daoScript collection) datum daoShare
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ NftData collection newNft
  Contract.logInfo @Hask.String $ printf "Change owner successful: %s" (Hask.show $ assetClass curr newName)
