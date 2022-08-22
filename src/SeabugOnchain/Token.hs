{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module SeabugOnchain.Token (
  mkPolicy,
  policyDataScript,
  policyData,
  mkTokenName,
) where

import Ledger (
  CurrencySymbol,
  Datum (Datum),
  MintingPolicy (MintingPolicy),
  PaymentPubKeyHash (unPaymentPubKeyHash),
  Script,
  ScriptContext,
  TxInfo (txInfoMint, txInfoOutputs),
  TxOut (TxOut, txOutAddress, txOutValue),
  ValidatorHash,
  findDatum,
  fromCompiledCode,
  minAdaTxOut,
  ownCurrencySymbol,
  pubKeyHashAddress,
  scriptContextTxInfo,
  txSignedBy,
 )
import Ledger.Ada qualified as Ada
import Ledger.Address (
  scriptHashAddress,
 )
import Ledger.Typed.Scripts (mkUntypedMintingPolicy)
import Ledger.Value (TokenName (TokenName), valueOf)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude
import SeabugOnchain.Types (
  MintAct (..),
  Natural,
  NftCollection (..),
  NftId,
  hash,
  nftId'collectionNftTn,
  nftId'owner,
  nftId'price,
 )

import PlutusTx (toData)

{-# INLINEABLE mkPolicyData #-}
mkPolicyData ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  MintAct ->
  ScriptContext ->
  Bool
mkPolicyData collectionNftCs lockingScript author authorShare daoScript daoShare =
  mkPolicy
    (PlutusTx.unsafeFromBuiltinData collectionNftCs)
    (PlutusTx.unsafeFromBuiltinData lockingScript)
    (PlutusTx.unsafeFromBuiltinData author)
    (PlutusTx.unsafeFromBuiltinData authorShare)
    (PlutusTx.unsafeFromBuiltinData daoScript)
    (PlutusTx.unsafeFromBuiltinData daoShare)

{-# INLINEABLE mkPolicy #-}
mkPolicy ::
  CurrencySymbol ->
  ValidatorHash ->
  PaymentPubKeyHash ->
  Natural ->
  ValidatorHash ->
  Natural ->
  MintAct ->
  ScriptContext ->
  Bool
mkPolicy collectionNftCs lockingScript author authorShare daoScript daoShare mintAct ctx =
  case mintAct of
    MintToken nft ->
      traceIfFalse "Exactly one NFT must be minted" (checkMint nft)
        && traceIfFalse "Underlying NFT must be locked" (checkCollectionNftBurned nft)
    ChangePrice nft newPrice ->
      traceIfFalse
        "Exactly one new token must be minted and exactly one old burnt"
        (checkMintAndBurn nft newPrice (nftId'owner nft))
        && traceIfFalse "Owner must sign the transaction" (txSignedBy info . unPaymentPubKeyHash . nftId'owner $ nft)
    ChangeOwner nft newOwner ->
      traceIfFalse
        "Exactly one new token must be minted and exactly one old burnt"
        (checkMintAndBurn nft (nftId'price nft) newOwner)
        && traceIfFalse "Royalities not paid" (zeroPrice nft || checkPartiesGotCorrectPayments nft)
    BurnToken nft ->
      traceIfFalse "NFT must be burned" (checkBurn nft)
        && traceIfFalse "Owner must sign the transaction" (txSignedBy info . unPaymentPubKeyHash . nftId'owner $ nft)
        && traceIfFalse "Underlying NFT must be unlocked" (checkUnlockNft nft)
  where
    !info = scriptContextTxInfo ctx
    info :: TxInfo
    -- ! force evaluation of `ownCs` causes policy compilation error
    ownCs = ownCurrencySymbol ctx
    ownCs :: CurrencySymbol

    !mintedValue = txInfoMint info

    -- Check if only one token is minted and name is correct
    checkMint :: NftId -> Bool
    checkMint nft =
      let newName = mkTokenName nft
          valMap = Value.getValue mintedValue
          tokens = Map.toList $ fromMaybe (traceError "unreachable") $ Map.lookup ownCs valMap
       in case tokens of
            [(tn, amt)] -> tn == newName && amt == 1
            _ -> False

    -- Check if the old token is burnt and new is minted with correct name
    checkMintAndBurn :: NftId -> Natural -> PaymentPubKeyHash -> Bool
    checkMintAndBurn nft newPrice newOwner =
      let minted = Map.toList <$> (Map.lookup ownCs . Value.getValue . txInfoMint $ info)
          oldName = mkTokenName nft
          newName = mkTokenName nft {nftId'price = newPrice, nftId'owner = newOwner}
       in case minted of
            Just [(tokenName1, tnAmt1), (tokenName2, tnAmt2)]
              | tokenName1 == oldName && tokenName2 == newName -> tnAmt1 == -1 && tnAmt2 == 1
              | tokenName2 == oldName && tokenName1 == newName -> tnAmt2 == -1 && tnAmt1 == 1
            _ -> False

    checkBurn :: NftId -> Bool
    checkBurn nft =
      let oldName = mkTokenName nft
          valMap = Value.getValue mintedValue
       in Map.singleton oldName (negate 1) == fromMaybe (traceError "unreachable") (Map.lookup ownCs valMap)

    -- Check if collection nft is burned
    checkCollectionNftBurned :: NftId -> Bool
    checkCollectionNftBurned nft =
      let lockingAddress = scriptHashAddress lockingScript
          containsCollectonNft tx =
            txOutAddress tx == lockingAddress
              && Value.valueOf (txOutValue tx) collectionNftCs (nftId'collectionNftTn nft) == 1
       in any containsCollectonNft (txInfoOutputs info)

    checkUnlockNft :: NftId -> Bool
    checkUnlockNft nft =
      let lockingAddress = scriptHashAddress lockingScript
          containsCollectonNft tx =
            txOutAddress tx /= lockingAddress
              && Value.valueOf (txOutValue tx) collectionNftCs (nftId'collectionNftTn nft) == 1
       in any containsCollectonNft (txInfoOutputs info)

    zeroPrice :: NftId -> Bool
    zeroPrice nft = nftId'price nft == zero

    -- Check that all parties received corresponding payments,
    -- and the payment utxos have the correct datum attached
    checkPartiesGotCorrectPayments :: NftId -> Bool
    checkPartiesGotCorrectPayments nft =
      let outs = txInfoOutputs info
          price' = fromEnum $ nftId'price nft
          royalty' = fromEnum authorShare
          mpShare = fromEnum daoShare

          shareToSubtract v
            | v < Ada.getLovelace minAdaTxOut = 0
            | otherwise = v

          authorAddr = pubKeyHashAddress author Nothing
          authorShareVal = (price' * royalty') `divide` 100_00

          daoAddr = scriptHashAddress daoScript
          daoShareVal = (price' * mpShare) `divide` 100_00

          ownerAddr = pubKeyHashAddress (nftId'owner nft) Nothing
          ownerShare = price' - shareToSubtract authorShareVal - shareToSubtract daoShareVal

          curSymDatum = Datum $ PlutusTx.toBuiltinData (ownCs, mkTokenName nft)

          -- Don't check royalties when lower than min ada
          filterLowValue v cond
            | v < Ada.getLovelace minAdaTxOut = True
            | otherwise = any (checkPaymentTxOut cond v) outs

          checkPaymentTxOut addr val (TxOut addr' val' dh) =
            addr == addr' && val == valueOf val' Ada.adaSymbol Ada.adaToken
              && (dh >>= \dh' -> findDatum dh' info) == Just curSymDatum
       in filterLowValue daoShareVal daoAddr
            && filterLowValue authorShareVal authorAddr
            && any (checkPaymentTxOut ownerAddr ownerShare) outs

{-# INLINEABLE mkTokenName #-}
mkTokenName :: NftId -> TokenName
mkTokenName = TokenName . hash

policyDataScript :: Script
policyDataScript = fromCompiledCode $$(PlutusTx.compile [||\a b c d e f -> mkUntypedMintingPolicy (mkPolicyData a b c d e f)||])

policyData :: NftCollection -> MintingPolicy
policyData NftCollection {..} =
  MintingPolicy $
    policyDataScript
      `Plutus.applyArguments` [ toData nftCollection'collectionNftCs
                              , toData nftCollection'lockingScript
                              , toData nftCollection'author
                              , toData nftCollection'authorShare
                              , toData nftCollection'daoScript
                              , toData nftCollection'daoShare
                              ]
