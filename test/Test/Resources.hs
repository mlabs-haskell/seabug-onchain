module Test.Resources (test) where

import Prelude hiding (fromEnum, toEnum)

import Control.Monad (void, (<=<))
import Control.Monad.State.Strict (modify)
import Data.Default (def)
import Data.Kind (Type)
import Data.List (find)
import Data.Maybe (fromJust)
import Ledger (
  Extended (Finite, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  PaymentPubKeyHash (PaymentPubKeyHash),
  PubKeyHash,
  UpperBound (UpperBound),
  Validator (Validator),
  minAdaTxOut,
  scriptCurrencySymbol,
  unPaymentPubKeyHash,
  unValidatorScript,
 )
import Ledger.Ada (getLovelace, lovelaceValueOf, toValue)
import Ledger.Typed.Scripts (Any, tvValidator, validatorHash)
import Ledger.Typed.Scripts qualified as P
import Ledger.Value (Value, assetClass, singleton, unAssetClass, valueOf)
import Plutus.Model (
  DatumMode (HashDatum),
  FakeCoin (FakeCoin),
  MockConfig,
  Run,
  TypedPolicy (TypedPolicy),
  TypedValidator (TypedValidator),
  TypedValidatorHash (TypedValidatorHash),
  boxAt,
  currentTime,
  fakeCoin,
  fakeValue,
  filterSlot,
  mintValue,
  mockConfig,
  mockConfigSlotConfig,
  newUser,
  payToKey,
  payToKeyDatum,
  payToScript,
  sendTx,
  signTx,
  spend,
  spendBox,
  testLimits,
  toV1,
  txBoxOut,
  userSpend,
  validateIn,
 )
import Plutus.V1.Ledger.Api (BuiltinData, toBuiltinData)
import Plutus.V2.Ledger.Api (txOutValue)
import PlutusTx.Enum (fromEnum, toEnum)
import PlutusTx.Prelude (divide)
import SeabugOnchain.Dao (daoValidator)
import SeabugOnchain.Lock (lockValidator)
import SeabugOnchain.Marketplace (marketplaceValidator)
import SeabugOnchain.Token (mkTokenName, policyData)
import SeabugOnchain.Types (
  LockAct (Unstake),
  LockDatum (LockDatum),
  MarketplaceDatum (MarketplaceDatum),
  MintAct (BurnToken, ChangeOwner, ChangePrice, MintToken),
  NftCollection (NftCollection),
  NftData (NftData),
  NftId (NftId),
  nftCollection'author,
  nftCollection'authorShare,
  nftCollection'collectionNftCs,
  nftCollection'daoScript,
  nftCollection'daoShare,
  nftCollection'lockLockup,
  nftCollection'lockLockupEnd,
  nftCollection'lockingScript,
  nftData'nftCollection,
  nftData'nftId,
  nftId'collectionNftTn,
  nftId'owner,
  nftId'price,
 )
import Test.Tasty (TestTree, testGroup)

test :: MockConfig -> TestTree
test cfg =
  testGroup
    "Resources usage"
    [ good "Seabug scripts" 3 seabugActions
    ]
  where
    good msg n =
      testLimits
        initFunds
        cfg
        msg
        (filterSlot (> n))

--  uncomment to see stats, it introduces fake error, script will fail but we can see the results
--  . (>> logError "Show stats")

cnftCoinA :: FakeCoin
cnftCoinA = FakeCoin "aa"

initFunds :: Value
initFunds = mconcat [lovelaceValueOf 300_000_000, fakeValue cnftCoinA 1]

psmTypedValidator ::
  forall (datum :: Type) (redeemer :: Type).
  P.TypedValidator Any ->
  TypedValidator datum redeemer
psmTypedValidator = TypedValidator . toV1 . Validator . unValidatorScript . tvValidator

marketplaceValidator' :: TypedValidator MarketplaceDatum BuiltinData
marketplaceValidator' = psmTypedValidator marketplaceValidator

seabugActions :: Run ()
seabugActions = do
  -- Use the same slot config as is used onchain
  modify (\mock -> mock {mockConfig = (mockConfig mock) {mockConfigSlotConfig = def}})

  w1 <- newUser $ lovelaceValueOf 100_000_000 <> fakeValue cnftCoinA 1
  w2 <- newUser $ lovelaceValueOf 100_000_000
  w3 <- newUser $ lovelaceValueOf 100_000_000

  mint w1 [w3] cnftCoinA 10_000_000
    >>= changePrice 8_000_000
    >>= marketplaceDeposit
    >>= marketplaceChangePrice 50_000_000
    >>= marketplaceBuy w2
    >>= marketplaceChangePrice 10_000_000
    >>= marketplaceRedeem
    >>= unstake
  feeWithdraw w3 [w3]

feeWithdraw :: PubKeyHash -> [PubKeyHash] -> Run ()
feeWithdraw pkh vaultKeys = do
  boxes <- boxAt daoValidator'
  let feeValue = foldMap (txOutValue . txBoxOut) boxes
  void
    . (sendTx <=< signTx pkh)
    . mconcat
    $ payToKey pkh feeValue :
    fmap (spendBox daoValidator' (toBuiltinData ())) boxes
  where
    daoValidator' :: TypedValidator BuiltinData BuiltinData
    daoValidator' = psmTypedValidator $ daoValidator vaultKeys

unstake :: NftData -> Run ()
unstake nftData = do
  box <- fromJust . find findCnft <$> boxAt lockValidator'
  utxos <- spend owner (singleton nftCS nftTN 1)
  now <- currentTime
  void
    . (sendTx <=< signTx owner <=< validateIn (range now))
    . mconcat
    $ [ spendBox lockValidator' (Unstake (PaymentPubKeyHash owner) (nftId'price nft)) box
      , mintValue policy' redeemer nftVal
      , userSpend utxos
      , payToKey owner $ singleton cnftCS cnftTN 1 <> toValue minAdaTxOut
      ]
  where
    findCnft box = valueOf (txOutValue . txBoxOut $ box) cnftCS cnftTN == 1
    redeemer = BurnToken nft
    policy = policyData collection
    policy' = TypedPolicy $ toV1 $ policyData collection
    owner = unPaymentPubKeyHash . nftId'owner $ nft
    nftCS = scriptCurrencySymbol policy
    nft = nftData'nftId nftData
    collection = nftData'nftCollection nftData
    nftTN = mkTokenName nft
    nftVal = singleton nftCS nftTN (-1)
    cnftCS = nftCollection'collectionNftCs collection
    cnftTN = nftId'collectionNftTn nft
    lockValidator' :: TypedValidator LockDatum LockAct
    lockValidator' = psmTypedValidator $ lockValidator cnftCS 5 5
    range now = Interval (LowerBound (Finite now) True) (UpperBound PosInf False)

marketplaceBuy :: PubKeyHash -> NftData -> Run NftData
marketplaceBuy newOwner nftData = do
  box <- fromJust . find findNft <$> boxAt marketplaceValidator'
  utxos <- spend newOwner (lovelaceValueOf . fromEnum . nftId'price . nftData'nftId $ nftData)
  void
    . (sendTx <=< signTx newOwner)
    . mconcat
    $ [ mintValue policy'' redeemer (newNftVal <> oldNftVal)
      , payToScript
          marketplaceValidator'
          (HashDatum $ MarketplaceDatum $ assetClass nftCS newNftTN)
          (newNftVal <> toValue minAdaTxOut)
      , spendBox marketplaceValidator' (toBuiltinData ()) box
      , payToKeyDatum oldOwner (HashDatum datum) (lovelaceValueOf ownerShare)
      , userSpend utxos
      ]
      <> filterLowValue
        authorShare
        (payToKeyDatum authorPkh (HashDatum datum) (lovelaceValueOf authorShare))
      <> filterLowValue
        daoShare
        (payToScript daoHash (HashDatum datum) (lovelaceValueOf daoShare))
  pure $ NftData (nftData'nftCollection nftData) newNft
  where
    filterLowValue v t
      | v < getLovelace minAdaTxOut = mempty
      | otherwise = pure t
    getShare share = (oldPrice * share) `divide` 10000
    authorShare :: Integer = getShare (fromEnum . nftCollection'authorShare . nftData'nftCollection $ nftData)
    daoShare = getShare (fromEnum . nftCollection'daoShare . nftData'nftCollection $ nftData)
    datum = toBuiltinData (nftCS, oldNftTN)
    findNft box = valueOf (txOutValue . txBoxOut $ box) nftCS oldNftTN == 1
    redeemer = ChangeOwner oldNft (PaymentPubKeyHash newOwner)
    policy' = policyData (nftData'nftCollection nftData)
    policy'' = TypedPolicy $ toV1 policy'
    nftCS = scriptCurrencySymbol policy'
    oldNft = nftData'nftId nftData
    oldNftTN = mkTokenName oldNft
    oldNftVal = singleton nftCS oldNftTN (-1)
    newNft = oldNft {nftId'owner = PaymentPubKeyHash newOwner}
    newNftTN = mkTokenName newNft
    newNftVal = singleton nftCS newNftTN 1
    oldOwner = unPaymentPubKeyHash . nftId'owner $ oldNft
    oldPrice = fromEnum . nftId'price $ oldNft
    filterLow x
      | x < getLovelace minAdaTxOut = 0
      | otherwise = x
    ownerShare = oldPrice - filterLow daoShare - filterLow authorShare
    authorPkh = unPaymentPubKeyHash . nftCollection'author . nftData'nftCollection $ nftData
    daoHash = TypedValidatorHash . toV1 . nftCollection'daoScript . nftData'nftCollection $ nftData

marketplaceChangePrice :: Integer -> NftData -> Run NftData
marketplaceChangePrice newPrice nftData = do
  box <- fromJust . find findNft <$> boxAt marketplaceValidator'
  void
    . (sendTx <=< signTx owner)
    . mconcat
    $ [ mintValue policy'' redeemer (newNftVal <> oldNftVal)
      , payToScript
          marketplaceValidator'
          (HashDatum $ MarketplaceDatum $ assetClass nftCS newNftTN)
          (newNftVal <> toValue minAdaTxOut)
      , spendBox marketplaceValidator' (toBuiltinData ()) box
      ]
  pure $ NftData (nftData'nftCollection nftData) newNft
  where
    findNft box = valueOf (txOutValue . txBoxOut $ box) nftCS oldNftTN == 1
    redeemer = ChangePrice oldNft (toEnum newPrice)
    policy' = policyData (nftData'nftCollection nftData)
    policy'' = TypedPolicy $ toV1 policy'
    nftCS = scriptCurrencySymbol policy'
    oldNft = nftData'nftId nftData
    oldNftTN = mkTokenName oldNft
    oldNftVal = singleton nftCS oldNftTN (-1)
    newNft = oldNft {nftId'price = toEnum newPrice}
    newNftTN = mkTokenName newNft
    newNftVal = singleton nftCS newNftTN 1
    owner = unPaymentPubKeyHash . nftId'owner $ oldNft

marketplaceRedeem :: NftData -> Run NftData
marketplaceRedeem nftData = do
  box <- fromJust . find findNft <$> boxAt marketplaceValidator'
  void
    . (sendTx <=< signTx owner)
    . mconcat
    $ [ mintValue policy'' redeemer (newNftVal <> oldNftVal)
      , payToKey owner (newNftVal <> toValue minAdaTxOut)
      , spendBox marketplaceValidator' (toBuiltinData ()) box
      ]
  pure $ NftData (nftData'nftCollection nftData) newNft
  where
    findNft box = valueOf (txOutValue . txBoxOut $ box) nftCS oldNftTN == 1
    redeemer = ChangePrice oldNft (toEnum newPrice)
    policy' = policyData (nftData'nftCollection nftData)
    policy'' = TypedPolicy $ toV1 policy'
    nftCS = scriptCurrencySymbol policy'
    oldNft = nftData'nftId nftData
    oldNftTN = mkTokenName oldNft
    oldNftVal = singleton nftCS oldNftTN (-1)
    newNft = oldNft {nftId'price = toEnum newPrice}
    newNftTN = mkTokenName newNft
    newNftVal = singleton nftCS newNftTN 1
    owner = unPaymentPubKeyHash . nftId'owner $ oldNft
    newPrice = subtract 1 . fromEnum . nftId'price $ oldNft

marketplaceDeposit :: NftData -> Run NftData
marketplaceDeposit nftData = do
  utxos <- spend owner (singleton nftCS nftTN 1 <> toValue minAdaTxOut)
  void
    . (sendTx <=< signTx owner)
    . mconcat
    $ [ payToScript
          marketplaceValidator'
          (HashDatum $ MarketplaceDatum $ assetClass nftCS nftTN)
          (nftVal <> toValue minAdaTxOut)
      , userSpend utxos
      ]
  pure nftData
  where
    policy' = policyData (nftData'nftCollection nftData)
    nftTN = mkTokenName . nftData'nftId $ nftData
    nftCS = scriptCurrencySymbol policy'
    nftVal = singleton nftCS nftTN 1
    owner = unPaymentPubKeyHash . nftId'owner . nftData'nftId $ nftData

changePrice :: Integer -> NftData -> Run NftData
changePrice newPrice nftData = do
  utxos <- spend owner (singleton nftCS oldNftTN 1 <> toValue minAdaTxOut)
  void
    . (sendTx <=< signTx owner)
    . mconcat
    $ [ mintValue policy'' redeemer (newNftVal <> oldNftVal)
      , payToKey owner (newNftVal <> toValue minAdaTxOut)
      , userSpend utxos
      ]
  pure $ NftData (nftData'nftCollection nftData) newNft
  where
    redeemer = ChangePrice oldNft (toEnum newPrice)
    policy' = policyData (nftData'nftCollection nftData)
    policy'' = TypedPolicy $ toV1 policy'
    nftCS = scriptCurrencySymbol policy'
    oldNft = nftData'nftId nftData
    oldNftTN = mkTokenName oldNft
    oldNftVal = singleton nftCS oldNftTN (-1)
    newNft = oldNft {nftId'price = toEnum newPrice}
    newNftTN = mkTokenName newNft
    newNftVal = singleton nftCS newNftTN 1
    owner = unPaymentPubKeyHash . nftId'owner . nftData'nftId $ nftData

mint :: PubKeyHash -> [PubKeyHash] -> FakeCoin -> Integer -> Run NftData
mint pkh vaultKeys cnftCoin price = do
  utxos <- spend pkh (cnftVal <> toValue minAdaTxOut)
  void
    . (sendTx <=< signTx pkh)
    . mconcat
    $ [ mintValue policy'' redeemer nftVal
      , payToScript lockScript' (HashDatum $ LockDatum nftCS 0 cnftTN) cnftVal
      , payToKey pkh (nftVal <> toValue minAdaTxOut)
      , userSpend utxos
      ]
  pure $ NftData collection nft
  where
    redeemer = MintToken nft
    lockScript = lockValidator cnftCS 5 5
    lockScript' :: TypedValidator LockDatum LockAct
    lockScript' = psmTypedValidator lockScript
    daoHash = validatorHash $ daoValidator vaultKeys
    collection =
      NftCollection
        { nftCollection'collectionNftCs = cnftCS
        , nftCollection'lockLockup = 5
        , nftCollection'lockLockupEnd = 5
        , nftCollection'lockingScript = validatorHash lockScript
        , nftCollection'author = PaymentPubKeyHash pkh
        , nftCollection'authorShare = toEnum 0
        , nftCollection'daoScript = daoHash
        , nftCollection'daoShare = toEnum 10_00
        }
    policy' = policyData collection
    policy'' = TypedPolicy $ toV1 policy'
    nft = NftId cnftTN (toEnum price) (PaymentPubKeyHash pkh)
    nftTN = mkTokenName nft
    nftCS = scriptCurrencySymbol policy'
    nftVal = singleton nftCS nftTN 1
    cnftTN = snd . unAssetClass . fakeCoin $ cnftCoin
    cnftCS = fst . unAssetClass . fakeCoin $ cnftCoin
    cnftVal = fakeValue cnftCoinA 1 <> toValue minAdaTxOut
