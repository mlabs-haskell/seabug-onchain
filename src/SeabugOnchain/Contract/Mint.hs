module SeabugOnchain.Contract.Mint (mint, mintWithCollection, generateNft) where

import PlutusTx.Prelude (
  Bool (False, True),
  Maybe (Nothing),
  Semigroup ((<>)),
  fromMaybe,
  fst,
  return,
  snd,
  ($),
  (.),
 )
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Aeson (toJSON)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text (pack)
import Data.Void (Void)
import Ledger (Datum (Datum), MintingPolicy (getMintingPolicy), Redeemer (Redeemer), minAdaTxOut, scriptHash, unPaymentPubKeyHash)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.Metadata (OtherFields (OtherFields), TxMetadata (TxMetadata))
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Ledger.Typed.Scripts (validatorHash)
import Plutus.Contract qualified as Contract
import Plutus.Contracts.Currency (CurrencyError, mintContract)
import Plutus.Contracts.Currency qualified as MC
import Plutus.V1.Ledger.Ada (lovelaceValueOf, toValue)
import Plutus.V1.Ledger.Api (Extended (Finite, PosInf), Interval (Interval), LowerBound (LowerBound), ToData (toBuiltinData), TokenName (TokenName), UpperBound (UpperBound))
import Plutus.V1.Ledger.Value (AssetClass, assetClass, assetClassValue, singleton, unAssetClass)
import Text.Printf (printf)

import SeabugOnchain.Contract.Aux (getUserUtxos)
import SeabugOnchain.Dao (daoValidator)
import SeabugOnchain.Lock (lockValidator)
import SeabugOnchain.Token (mkTokenName, policyData)
import SeabugOnchain.Types

mint :: MintParams -> UserContract NftData
mint mp = do
  ac <- generateNft
  mintWithCollection (ac, mp)

mintWithCollection :: (AssetClass, MintParams) -> UserContract NftData
mintWithCollection (ac, mp) = do
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- getUserUtxos
  currSlot <- Contract.currentSlot
  Contract.logInfo @Hask.String $ printf "Curr slot: %s" (Hask.show currSlot)
  let owner = fromMaybe (pkh, Nothing) (mp'owner mp)
      now = slotToBeginPOSIXTime def currSlot
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
      seabugMeta =
        SeabugMetadata
          { sm'policyId = scriptHash . getMintingPolicy $ policy'
          , sm'mintPolicy = mp'mintPolicy mp
          , sm'collectionNftCS = nftCollection'collectionNftCs collection
          , sm'collectionNftTN = nftId'collectionNftTn nft
          , sm'lockingScript = nftCollection'lockingScript collection
          , sm'authorPkh = unPaymentPubKeyHash $ nftCollection'author collection
          , sm'authorShare = nftCollection'authorShare collection
          , sm'marketplaceScript = nftCollection'daoScript collection
          , sm'marketplaceShare = nftCollection'daoShare collection
          , sm'ownerPkh = unPaymentPubKeyHash $ nftId'owner nft
          , sm'ownerPrice = nftId'price nft
          }

      meta = TxMetadata Nothing $ OtherFields $ Map.singleton "727" $ toJSON seabugMeta
      lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
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
          , Constraints.mustIncludeMetadata meta
          , Constraints.mustPayWithDatumToPubKey pkh (Datum $ toBuiltinData ()) (lovelaceValueOf 5_000_000)
          , Constraints.mustPayWithDatumToPubKey pkh (Datum $ toBuiltinData ()) (lovelaceValueOf 5_000_000)
          , Constraints.mustPayWithDatumToPubKey pkh (Datum $ toBuiltinData ()) (lovelaceValueOf 5_000_000)
          , Constraints.mustPayWithDatumToPubKey pkh (Datum $ toBuiltinData ()) (lovelaceValueOf 5_000_000)
          ]
  void $ Contract.submitTxConstraintsWith @Void lookup tx
  Contract.tell . Hask.pure $ nftData
  Contract.logInfo @Hask.String $ Hask.show nft
  Contract.logInfo @Hask.String $ printf "Mint successful: %s" (Hask.show $ assetClass curr tn)
  Hask.pure nftData

generateNft :: UserContract AssetClass
generateNft = do
  self <- Contract.ownPaymentPubKeyHash
  let tn = TokenName "NFT"
  x <-
    Contract.mapError
      (pack . Hask.show @CurrencyError)
      (mintContract self [(tn, 1)])
  return $ assetClass (MC.currencySymbol x) tn
