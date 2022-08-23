{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SeabugOnchain.Types (
  GenericContract,
  UserContract,
  Natural (UnsafeMkNatural),
  NFTAppSchema,
  MintParams (..),
  NftId (..),
  NftCollection (..),
  NftData (..),
  SetPriceParams (..),
  ChangeOwnerParams (..),
  MintAct (..),
  Hashable (..),
  LockAct (..),
  LockDatum (..),
  MarketplaceDatum (..),
  MintCnftParams (..),
  SeabugMetadata (..),
) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (decodeUtf8)
import Prelude qualified as Hask

import Cardano.Prelude (decodeUtf8)
import Data.Aeson (FromJSON (parseJSON), ToJSON, object, toJSON, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (parseFail)
import Data.ByteString.Base16 (encode)
import Data.Data (Data)
import Data.Monoid (Last)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), PubKeyHash (getPubKeyHash), ScriptHash (getScriptHash), Slot, StakePubKeyHash, ValidatorHash (ValidatorHash))
import Plutus.Contract (Contract, Endpoint, type (.\/))
import Plutus.V1.Ledger.Api (fromBuiltinData, toBuiltinData, unCurrencySymbol, unTokenName, unsafeFromBuiltinData)
import Plutus.V1.Ledger.Crypto (PubKeyHash (PubKeyHash))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), CurrencySymbol (CurrencySymbol), TokenName (TokenName))
import PlutusTx.Builtins (blake2b_256, matchData', matchList)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString), equalsInteger, ifThenElse, mkCons, mkConstr, mkNilData, unitval, unsafeDataAsConstr)
import PlutusTx.Builtins.Internal qualified as Internal
import PlutusTx.ErrorCodes (reconstructCaseError)
import Schema (ToSchema)
import Type.Reflection (Typeable)

-- Instances for Data
deriving stock instance (Data PubKeyHash)
deriving stock instance (Data PaymentPubKeyHash)
deriving stock instance (Data ValidatorHash)

newtype Natural = UnsafeMkNatural Integer
  deriving newtype (ToJSON, AdditiveSemigroup, AdditiveMonoid, MultiplicativeSemigroup, MultiplicativeMonoid)
  deriving stock (Hask.Show, Hask.Eq, Generic, Hask.Ord, Typeable, Data)
  deriving anyclass (ToSchema)

PlutusTx.makeLift ''Natural

instance Eq Natural where
  {-# INLINEABLE (==) #-}
  UnsafeMkNatural x == UnsafeMkNatural y = x == y

instance FromJSON Natural where
  parseJSON v = do
    i <- parseJSON v
    if i < 0 then parseFail "Natural less than 0" else Hask.pure (UnsafeMkNatural i)

instance Enum Natural where
  {-# INLINEABLE toEnum #-}
  toEnum x
    | x < 0 = error ()
    | otherwise = UnsafeMkNatural x
  {-# INLINEABLE fromEnum #-}
  fromEnum (UnsafeMkNatural x) = x
  {-# INLINEABLE succ #-}
  succ (UnsafeMkNatural x) = UnsafeMkNatural $ succ x
  {-# INLINEABLE pred #-}
  pred (UnsafeMkNatural x) = if x == 0 then error () else UnsafeMkNatural $ pred x

instance PlutusTx.ToData Natural where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UnsafeMkNatural x) = toBuiltinData x

instance PlutusTx.FromData Natural where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData bData =
    fromBuiltinData bData >>= (\x -> if x < 0 then Nothing else Just $ UnsafeMkNatural x)

instance PlutusTx.UnsafeFromData Natural where
  {-# INLINEABLE PlutusTx.unsafeFromBuiltinData #-}
  unsafeFromBuiltinData bData = toEnum $ unsafeFromBuiltinData bData

-- | Parameters that need to be submitted when minting a new NFT.
data MintParams = MintParams
  { -- | Shares retained by author.
    mp'authorShare :: Natural
  , mp'daoShare :: Natural
  , -- | Listing price of the NFT, in Lovelace.
    mp'price :: Natural
  , mp'lockLockup :: Integer
  , mp'lockLockupEnd :: Slot
  , mp'fakeAuthor :: Maybe PaymentPubKeyHash
  , mp'feeVaultKeys :: [PubKeyHash]
  , mp'owner :: Maybe (PaymentPubKeyHash, Maybe StakePubKeyHash)
  , mp'mintPolicy :: Hask.String
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data NftId = NftId
  { nftId'collectionNftTn :: TokenName
  , nftId'price :: Natural
  , nftId'owner :: PaymentPubKeyHash
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord, Typeable, Data)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance PlutusTx.ToData NftId where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (NftId tn price owner) =
    mkConstr 0 $
      mkCons (toBuiltinData tn) $
        mkCons (toBuiltinData price) $
          mkCons (toBuiltinData owner) $
            mkNilData unitval

instance PlutusTx.FromData NftId where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dData =
    let cons3 collectionTn price owner lst =
          matchList
            lst
            ( pure NftId
                <*> fromBuiltinData collectionTn
                <*> fromBuiltinData price
                <*> fromBuiltinData owner
            )
            (\_ _ -> Nothing)
        cons2 collectionTn price lst =
          matchList
            lst
            Nothing
            (cons3 collectionTn price)
        cons1 collectionTn lst =
          matchList
            lst
            Nothing
            (cons2 collectionTn)
        cons0 lst =
          matchList
            lst
            Nothing
            cons1
        matchCase constrIndex lst =
          ifThenElse
            (constrIndex `equalsInteger` 0)
            (Hask.const (cons0 lst))
            (Hask.const Nothing)
            unitval
     in matchData'
          dData
          matchCase
          (Hask.const Nothing)
          (Hask.const Nothing)
          (Hask.const Nothing)
          (Hask.const Nothing)

instance PlutusTx.UnsafeFromData NftId where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData bData =
    let constr = unsafeDataAsConstr bData
        constrIndex = Internal.fst constr
        lst1 = Internal.snd constr
        lst2 = Internal.tail lst1
        lst3 = Internal.tail lst2
        collectionTn = Internal.head lst1
        price = Internal.head lst2
        owner = Internal.head lst3
        val =
          NftId
            (unsafeFromBuiltinData collectionTn)
            (unsafeFromBuiltinData price)
            (unsafeFromBuiltinData owner)
     in ifThenElse
          (constrIndex `equalsInteger` 0)
          (Hask.const val)
          (Hask.const (traceError reconstructCaseError))
          unitval

data NftCollection = NftCollection
  { nftCollection'collectionNftCs :: CurrencySymbol
  , nftCollection'lockLockup :: Integer
  , nftCollection'lockLockupEnd :: Slot
  , nftCollection'lockingScript :: ValidatorHash
  , nftCollection'author :: PaymentPubKeyHash
  , nftCollection'authorShare :: Natural
  , nftCollection'daoScript :: ValidatorHash
  , nftCollection'daoShare :: Natural
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord, Typeable, Data)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data NftData = NftData
  { nftData'nftCollection :: NftCollection
  , nftData'nftId :: NftId
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord, Typeable, Data)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SetPriceParams = SetPriceParams
  { -- | Token which price is set.
    sp'nftData :: NftData
  , -- | New price, in Lovelace.
    sp'price :: Natural
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data ChangeOwnerParams = ChangeOwnerParams
  { -- | Token which owner is set.
    cp'nftData :: NftData
  , -- | New Owner
    cp'owner :: PaymentPubKeyHash
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data MintAct
  = MintToken NftId
  | ChangePrice NftId Natural
  | ChangeOwner NftId PaymentPubKeyHash
  | BurnToken NftId
  deriving stock (Hask.Show)

instance PlutusTx.ToData MintAct where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (MintToken nft) =
    mkConstr 0 $
      mkCons (toBuiltinData nft) $
        mkNilData unitval
  toBuiltinData (ChangePrice nft price) =
    mkConstr 1 $
      mkCons (toBuiltinData nft) $
        mkCons (toBuiltinData price) $
          mkNilData unitval
  toBuiltinData (ChangeOwner nft pkh) =
    mkConstr 2 $
      mkCons (toBuiltinData nft) $
        mkCons (toBuiltinData pkh) $
          mkNilData unitval
  toBuiltinData (BurnToken nft) =
    mkConstr 3 $
      mkCons (toBuiltinData nft) $
        mkNilData unitval

instance PlutusTx.FromData MintAct where
  {-# INLINEABLE PlutusTx.fromBuiltinData #-}
  fromBuiltinData bData =
    let matchMintToken constrIndex lst =
          ifThenElse
            (constrIndex `equalsInteger` 0)
            (Hask.const (consMintToken0 lst))
            (Hask.const (matchChangePrice constrIndex lst))
            unitval
        consMintToken0 lst =
          matchList
            lst
            Nothing
            consMintToken1
        consMintToken1 nft lst =
          matchList
            lst
            ( pure MintToken
                <*> fromBuiltinData nft
            )
            (\_ _ -> Nothing)
        matchChangePrice constrIndex lst =
          ifThenElse
            (constrIndex `equalsInteger` 1)
            (Hask.const (consChangePrice0 lst))
            (Hask.const (matchChangeOwner constrIndex lst))
            unitval
        consChangePrice0 lst =
          matchList
            lst
            Nothing
            consChangePrice1
        consChangePrice1 nft lst =
          matchList
            lst
            Nothing
            (consChangePrice2 nft)
        consChangePrice2 nft price lst =
          matchList
            lst
            ( pure ChangePrice
                <*> fromBuiltinData nft
                <*> fromBuiltinData price
            )
            (\_ _ -> Nothing)
        matchChangeOwner constrIndex lst =
          ifThenElse
            (constrIndex `equalsInteger` 2)
            (Hask.const (consChangeOwner0 lst))
            (Hask.const (matchBurnToken constrIndex lst))
            unitval
        consChangeOwner0 lst =
          matchList
            lst
            Nothing
            consChangeOwner1
        consChangeOwner1 nft lst =
          matchList
            lst
            Nothing
            (consChangeOwner2 nft)
        consChangeOwner2 nft pkh lst =
          matchList
            lst
            ( pure ChangeOwner
                <*> fromBuiltinData nft
                <*> fromBuiltinData pkh
            )
            (\_ _ -> Nothing)
        matchBurnToken constrIndex lst =
          ifThenElse
            (constrIndex `equalsInteger` 3)
            (Hask.const (consBurnToken0 lst))
            (Hask.const (traceError reconstructCaseError))
            unitval
        consBurnToken0 lst =
          matchList
            lst
            Nothing
            consBurnToken1
        consBurnToken1 nft lst =
          matchList
            lst
            ( pure BurnToken
                <*> fromBuiltinData nft
            )
            (\_ _ -> Nothing)
     in matchData'
          bData
          matchMintToken
          (Hask.const Nothing)
          (Hask.const Nothing)
          (Hask.const Nothing)
          (Hask.const Nothing)

instance PlutusTx.UnsafeFromData MintAct where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData bData =
    let constr = unsafeDataAsConstr bData
        constrIndex = Internal.fst constr
        lst1 = Internal.snd constr
        nft = Internal.head lst1
        matchMintToken = MintToken (unsafeFromBuiltinData nft)
        matchBurnToken = BurnToken (unsafeFromBuiltinData nft)
        fallthrough1 =
          ifThenElse
            (constrIndex `equalsInteger` 3)
            (Hask.const matchBurnToken)
            (Hask.const fallthrough2)
            unitval
        fallthrough2 =
          let lst2 = Internal.tail lst1
              priceOrPkh = Internal.head lst2
              matchChangePrice =
                ChangePrice
                  (unsafeFromBuiltinData nft)
                  (unsafeFromBuiltinData priceOrPkh)
              matchChangeOwner =
                ChangeOwner
                  (unsafeFromBuiltinData nft)
                  (unsafeFromBuiltinData priceOrPkh)
              fallthrough3 =
                ifThenElse
                  (constrIndex `equalsInteger` 2)
                  (Hask.const matchChangeOwner)
                  (Hask.const (traceError reconstructCaseError))
                  unitval
           in ifThenElse
                (constrIndex `equalsInteger` 1)
                (Hask.const matchChangePrice)
                (Hask.const fallthrough3)
                unitval
     in ifThenElse
          (constrIndex `equalsInteger` 0)
          (Hask.const matchMintToken)
          (Hask.const fallthrough1)
          unitval

data LockAct
  = Unstake PaymentPubKeyHash Natural
  | Restake PaymentPubKeyHash Natural
  deriving stock (Hask.Show)

instance PlutusTx.ToData LockAct where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Unstake pkh price) =
    mkConstr 0 $
      mkCons (toBuiltinData pkh) $
        mkCons (toBuiltinData price) $
          mkNilData unitval
  toBuiltinData (Restake pkh price) =
    mkConstr 1 $
      mkCons (toBuiltinData pkh) $
        mkCons (toBuiltinData price) $
          mkNilData unitval

instance PlutusTx.FromData LockAct where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData bData =
    let matchUnstake constrIndex lst =
          ifThenElse
            (constrIndex `equalsInteger` 0)
            (Hask.const (cons0 Unstake lst))
            (Hask.const (matchRestake constrIndex lst))
            unitval
        matchRestake constrIndex lst =
          ifThenElse
            (constrIndex `equalsInteger` 1)
            (Hask.const (cons0 Restake lst))
            (Hask.const Nothing)
            unitval
        cons0 f lst =
          matchList
            lst
            Nothing
            (cons1 f)
        cons1 f pkh lst =
          matchList
            lst
            Nothing
            (cons2 f pkh)
        cons2 f pkh price lst =
          matchList
            lst
            ( pure f
                <*> fromBuiltinData pkh
                <*> fromBuiltinData price
            )
            (\_ _ -> Nothing)
     in matchData'
          bData
          matchUnstake
          (Hask.const Nothing)
          (Hask.const Nothing)
          (Hask.const Nothing)
          (Hask.const Nothing)

instance PlutusTx.UnsafeFromData LockAct where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData bData =
    let constr = unsafeDataAsConstr bData
        constrIndex = Internal.fst constr
        lst1 = Internal.snd constr
        lst2 = Internal.tail lst1
        pkh = Internal.head lst1
        price = Internal.head lst2
        restake =
          Restake
            (unsafeFromBuiltinData pkh)
            (unsafeFromBuiltinData price)
        unstake =
          Unstake
            (unsafeFromBuiltinData pkh)
            (unsafeFromBuiltinData price)
        fallthrough =
          ifThenElse
            (constrIndex `equalsInteger` 1)
            (Hask.const restake)
            (Hask.const (traceError reconstructCaseError))
            unitval
     in ifThenElse
          (constrIndex `equalsInteger` 0)
          (Hask.const unstake)
          (Hask.const fallthrough)
          unitval

data LockDatum = LockDatum
  { ld'sgNft :: CurrencySymbol
  , ld'entered :: Slot
  , ld'underlyingTn :: TokenName
  }
  deriving stock (Hask.Show)

instance PlutusTx.ToData LockDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (LockDatum sgNft entered underlyingTn) =
    mkConstr 0 $
      mkCons (toBuiltinData sgNft) $
        mkCons (toBuiltinData entered) $
          mkCons (toBuiltinData underlyingTn) $
            mkNilData unitval

instance PlutusTx.FromData LockDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData bData =
    let cons3 sgNft' entered' underlyingTn lst =
          matchList
            lst
            ( pure LockDatum
                <*> fromBuiltinData sgNft'
                <*> fromBuiltinData entered'
                <*> fromBuiltinData underlyingTn
            )
            (\_ _ -> Nothing)
        cons2 sgNft' entered lst =
          matchList
            lst
            Nothing
            (cons3 sgNft' entered)
        cons1 sgNft lst =
          matchList
            lst
            Nothing
            (cons2 sgNft)
        cons0 lst =
          matchList
            lst
            Nothing
            cons1
        matchCase constrIndex lst =
          ifThenElse
            (constrIndex `equalsInteger` 0)
            (Hask.const (cons0 lst))
            (Hask.const Nothing)
            unitval
     in matchData'
          bData
          matchCase
          (Hask.const Nothing)
          (Hask.const Nothing)
          (Hask.const Nothing)
          (Hask.const Nothing)

instance PlutusTx.UnsafeFromData LockDatum where
  {-# INLINEABLE PlutusTx.unsafeFromBuiltinData #-}
  unsafeFromBuiltinData bData =
    let constr = unsafeDataAsConstr bData
        constrIndex = Internal.fst constr
        lst1 = Internal.snd constr
        lst2 = Internal.tail lst1
        lst3 = Internal.tail lst2
        sgNft = Internal.head lst1
        entered = Internal.head lst2
        underlyingTn = Internal.head lst3
        val =
          LockDatum
            (PlutusTx.unsafeFromBuiltinData sgNft)
            (PlutusTx.unsafeFromBuiltinData entered)
            (PlutusTx.unsafeFromBuiltinData underlyingTn)
     in ifThenElse
          (constrIndex `equalsInteger` 0)
          (Hask.const val)
          (Hask.const (traceError reconstructCaseError))
          PlutusTx.Builtins.Internal.unitval

instance Eq LockDatum where
  {-# INLINEABLE (==) #-}
  LockDatum a b c == LockDatum a' b' c' = a == a' && b == b' && c == c'

newtype MarketplaceDatum = MarketplaceDatum {getMarketplaceDatum :: AssetClass}
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

class Hashable a where
  hash :: a -> BuiltinByteString

instance Hashable BuiltinByteString where
  {-# INLINEABLE hash #-}
  hash = blake2b_256

instance Hashable Natural where
  {-# INLINEABLE hash #-}
  hash = hash . toBin . fromEnum
    where
      {-# INLINEABLE toBin #-}
      toBin :: Integer -> BuiltinByteString
      toBin n = toBin' n mempty
        where
          toBin' n' rest
            | n' < 256 = consByteString n' rest
            | otherwise = toBin' (n' `divide` 256) (consByteString (n' `modulo` 256) rest)

instance (Hashable a, Hashable b) => Hashable (a, b) where
  hash (a, b) = hash (hash a <> hash b)

deriving via BuiltinByteString instance Hashable ValidatorHash
deriving via BuiltinByteString instance Hashable PaymentPubKeyHash
deriving via BuiltinByteString instance Hashable TokenName
deriving via BuiltinByteString instance Hashable CurrencySymbol
deriving via (CurrencySymbol, TokenName) instance Hashable AssetClass

instance Hashable NftId where
  {-# INLINEABLE hash #-}
  hash nft =
    hash $
      mconcat
        [ hash $ nftId'price nft
        , hash $ nftId'owner nft
        , hash $ nftId'collectionNftTn nft
        ]

data MintCnftParams = MintCnftParams
  { mc'image :: Text
  , mc'tokenName :: BuiltinByteString
  , mc'name :: Text
  , mc'description :: Text
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data SeabugMetadata = SeabugMetadata
  { -- | applied script hash
    sm'policyId :: ScriptHash
  , -- | arbitrary string for mintPolicy version identification
    sm'mintPolicy :: Hask.String
  , sm'collectionNftCS :: CurrencySymbol
  , sm'collectionNftTN :: TokenName
  , sm'lockingScript :: ValidatorHash
  , sm'authorPkh :: PubKeyHash
  , sm'authorShare :: Natural
  , sm'marketplaceScript :: ValidatorHash
  , sm'marketplaceShare :: Natural
  , sm'ownerPkh :: PubKeyHash
  , sm'ownerPrice :: Natural
  }

instance ToJSON SeabugMetadata where
  toJSON SeabugMetadata {..} =
    object
      [ (fromText . toHex . getScriptHash $ sm'policyId)
          .= object
            [ "mintPolicy" .= sm'mintPolicy
            , "collectionNftCS" .= unCurrencySymbol sm'collectionNftCS
            , "collectionNftTN" .= unTokenName sm'collectionNftTN
            , "lockingScript" .= sm'lockingScript
            , "authorPkh" .= getPubKeyHash sm'authorPkh
            , "authorShare" .= sm'authorShare
            , "marketplaceScript" .= sm'marketplaceScript
            , "marketplaceShare" .= sm'marketplaceShare
            , "ownerPkh" .= getPubKeyHash sm'ownerPkh
            , "ownerPrice" .= sm'ownerPrice
            ]
      ]
    where
      toHex :: BuiltinByteString -> Text
      toHex (BuiltinByteString str) = decodeUtf8 (encode str)

-- | A common App schema works for now.
type NFTAppSchema =
  -- Author Endpoints
  Endpoint "mint" MintParams
    .\/ Endpoint "mint-with-collection" (AssetClass, MintParams)
    -- User Action Endpoints
    .\/ Endpoint "change-owner" ChangeOwnerParams
    .\/ Endpoint "set-price" SetPriceParams
    .\/ Endpoint "marketplace-deposit" NftData
    .\/ Endpoint "marketplace-redeem" NftData
    .\/ Endpoint "marketplace-buy" NftData
    .\/ Endpoint "marketplace-set-price" SetPriceParams
    .\/ Endpoint "burn" NftData
    .\/ Endpoint "fee-withdraw" [PubKeyHash]
    .\/ Endpoint "mint-cnft" [MintCnftParams]

type GenericContract a = forall w s. Contract w s Text a
type UserContract a = Contract (Last NftData) NFTAppSchema Text a
