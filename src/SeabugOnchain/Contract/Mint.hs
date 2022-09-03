module SeabugOnchain.Contract.Mint (mint, mintWithCollection, generateNft) where

import PlutusTx.Prelude (
  Bool (False, True),
  Integer,
  Maybe (Nothing),
  Semigroup ((<>)),
  fromMaybe,
  fst,
  pure,
  return,
  snd,
  ($),
  (.),
 )
import Prelude qualified as Hask

import Control.Monad (void)

import Data.Map qualified as Map
import Data.Void (Void)

import Ledger (Datum (Datum), Redeemer (Redeemer), TxOutRef (TxOutRef), getCardanoTxId, minAdaTxOut, scriptCurrencySymbol)
import Ledger.Ada (toValue)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorHash)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract
import Plutus.Contract qualified as Contract
import Plutus.Contracts.Currency hiding (mintContract)
import Plutus.V1.Ledger.Api (Extended (Finite, PosInf), Interval (Interval), LowerBound (LowerBound), ToData (toBuiltinData), TokenName (TokenName), UpperBound (UpperBound))
import Plutus.V1.Ledger.Value (AssetClass, assetClass, assetClassValue, singleton, unAssetClass)
import PlutusTx.AssocMap qualified as AssocMap
import SeabugOnchain.Contract.Aux (getFirstUtxo, getUserUtxos)
import SeabugOnchain.Dao (daoValidator)
import SeabugOnchain.Lock (lockValidator)
import SeabugOnchain.Token (mkTokenName, policyData)
import SeabugOnchain.Types
import Text.Printf (printf)

mint :: MintParams -> UserContract NftData
mint mp = do
  ac <- generateNft
  mintWithCollection (ac, mp)

mintWithCollection :: (AssetClass, MintParams) -> UserContract NftData
mintWithCollection (ac, mp) = do
  Contract.logError @Hask.String "Before pkh"
  pkh <- Contract.ownFirstPaymentPubKeyHash
  Contract.logError @Hask.String "Before utxos"
  utxos <- getUserUtxos
  Contract.logError @Hask.String "Before currslot"
  currSlot <- Contract.currentPABSlot
  Contract.logError @Hask.String "Before now"
  now <- Contract.currentTime
  Contract.logInfo @Hask.String $ printf "Curr slot: %s" (Hask.show currSlot)
  let owner = fromMaybe (pkh, Nothing) (mp'owner mp)
      author = fromMaybe pkh $ mp'fakeAuthor mp
      nft =
        NftId
          { nftId'price = mp'price mp
          , nftId'owner = fst owner
          , nftId'collectionNftTn = snd . unAssetClass $ ac
          }
      collection =
        NftCollection
          { nftCollection'collectionNftCs = fst . unAssetClass $ ac
          , nftCollection'lockLockup = mp'lockLockup mp
          , nftCollection'lockLockupEnd = mp'lockLockupEnd mp
          , nftCollection'lockingScript =
              validatorHash $ lockValidator (fst $ unAssetClass ac) (mp'lockLockup mp) (mp'lockLockupEnd mp)
          , nftCollection'author = author
          , nftCollection'authorShare = mp'authorShare mp
          , nftCollection'daoScript = validatorHash $ daoValidator $ mp'feeVaultKeys mp
          , nftCollection'daoShare = mp'daoShare mp
          }
      policy' = policyData collection
      curr = scriptCurrencySymbol policy'
      tn = mkTokenName nft
      nftValue = singleton curr tn 1
      mintRedeemer = Redeemer . toBuiltinData . MintToken $ nft
      nftData = NftData collection nft
      -- seabugMeta =
      --   SeabugMetadata
      --     { sm'policyId = scriptHash . getMintingPolicy $ policy'
      --     , sm'mintPolicy = mp'mintPolicy mp
      --     , sm'collectionNftCS = nftCollection'collectionNftCs collection
      --     , sm'collectionNftTN = nftId'collectionNftTn nft
      --     , sm'lockingScript = nftCollection'lockingScript collection
      --     , sm'authorPkh = unPaymentPubKeyHash $ nftCollection'author collection
      --     , sm'authorShare = nftCollection'authorShare collection
      --     , sm'marketplaceScript = nftCollection'daoScript collection
      --     , sm'marketplaceShare = nftCollection'daoShare collection
      --     , sm'ownerPkh = unPaymentPubKeyHash $ nftId'owner nft
      --     , sm'ownerPrice = nftId'price nft
      --     }

      -- meta = TxMetadata Nothing $ OtherFields $ Map.singleton "727" $ toJSON seabugMeta
      lookup =
        Hask.mconcat
          [ Constraints.plutusV1MintingPolicy policy'
          , Constraints.unspentOutputs utxos
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer nftValue
          , Constraints.mustPayToPubKey pkh (nftValue <> toValue minAdaTxOut)
          , Constraints.mustPayToOtherScript
              (nftCollection'lockingScript collection)
              (Datum $ toBuiltinData $ LockDatum curr currSlot (snd $ unAssetClass ac))
              (assetClassValue ac 1 <> toValue minAdaTxOut)
          , Constraints.mustValidateIn $
              Interval
                (LowerBound (Finite now) True)
                (UpperBound PosInf False)
                -- , Constraints.mustIncludeMetadata meta
          ]
  Contract.logError @Hask.String "Before submitTx"
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ nftData
  Contract.logInfo @Hask.String $ Hask.show nft
  Contract.logInfo @Hask.String $ printf "Mint successful: %s" (Hask.show $ assetClass curr tn)
  Hask.pure nftData

generateNft :: UserContract AssetClass
generateNft = do
  let tn = TokenName "NFT"
  Contract.logError @Hask.String "Before mintContract"
  x <- mintContract [(tn, 1)]
  return $ assetClass (currencySymbol x) tn

mintContract ::
  [(TokenName, Integer)] ->
  UserContract OneShotCurrency
mintContract amounts = do
  utxos <- getUserUtxos
  Contract.logError @Hask.String $ Hask.show utxos
  utxo <- getFirstUtxo
  let theCurrency = mkCurrency (fst utxo) amounts
      curVali = curPolicy theCurrency
      lookups =
        Constraints.plutusV1MintingPolicy curVali
          Hask.<> Constraints.unspentOutputs (Map.insert (fst utxo) (snd utxo) utxos)
      mintTx =
        Constraints.mustSpendPubKeyOutput (fst utxo)
          Hask.<> Constraints.mustMintValue (mintedValue theCurrency)
  tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
  _ <- awaitTxConfirmed (getCardanoTxId tx)
  pure theCurrency

mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> OneShotCurrency
mkCurrency (TxOutRef h i) amts =
  OneShotCurrency
    { curRefTransactionOutput = (h, i)
    , curAmounts = AssocMap.fromList amts
    }
