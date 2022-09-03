{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Plutip (test) where

import Control.Monad (void)
import Control.Monad.Reader (ReaderT)
import Data.Bifunctor (second)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Last)
import Data.Semigroup (stimes)
import Data.Text (Text)
import Debug.Trace (traceShowId)
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), Value)
import Plutus.Contract (waitNSlots)
import Plutus.Contract qualified as Contract
import PlutusTx.Prelude (toEnum)
import SeabugOnchain.Contract.Burn (burn)
import SeabugOnchain.Contract.FeeWithdraw (feeWithdraw)
import SeabugOnchain.Contract.MarketplaceBuy (marketplaceBuy)
import SeabugOnchain.Contract.MarketplaceDeposit (marketplaceDeposit)
import SeabugOnchain.Contract.MarketplaceRedeem (marketplaceRedeem)
import SeabugOnchain.Contract.MarketplaceSetPrice (marketplaceSetPrice)
import SeabugOnchain.Contract.Mint (generateNft, mintWithCollection)
import SeabugOnchain.Contract.SetPrice (setPrice)
import SeabugOnchain.Types (MintParams (MintParams), NftData, SetPriceParams (SetPriceParams))
import Test.Plutip.Contract (TestWallets, assertExecution, initAda, withContractAs)
import Test.Plutip.Internal.Types (ClusterEnv, ExecutionResult (outcome), FailureReason)
import Test.Plutip.LocalCluster (BpiWallet, withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)
import Prelude hiding (toEnum)

doubleUTxOAda :: Integer -> TestWallets
doubleUTxOAda = flip stimes $ initAda [100, 5, 5, 5, 5]

outcomeResult :: forall (w :: Type) (e :: Type) (a :: Type). (Show e, Show a) => ExecutionResult w e (a, NonEmpty Value) -> Either (FailureReason e) a
outcomeResult = traceShowId . second fst . outcome

-- TODO: Partial value asserts here when added (https://github.com/mlabs-haskell/plutip/issues/42)
test :: TestTree
test =
  withCluster
    "Integration tests"
    [ assertExecution "Happy path" (doubleUTxOAda 3) testValid [shouldSucceed]
    , assertExecution "Gift path" (doubleUTxOAda 2) testGift [shouldSucceed]
    , assertExecution "Fail to change price when not owner" (doubleUTxOAda 2) testChangePriceNotOwner [shouldFail]
    , assertExecution "Fail to redeem when not owner" (doubleUTxOAda 2) testRedeemNotOwner [shouldFail]
    , assertExecution "Fail unlocking too early" (doubleUTxOAda 1) testBurnTooEarly [shouldFail]
    ]

type TestCase = ReaderT (ClusterEnv, NonEmpty BpiWallet) IO (ExecutionResult (Last NftData) Text ((), NonEmpty Value))

testValid :: TestCase
testValid = do
  (outcomeResult -> Right (nft3, pkhs)) <- withContractAs 0 $ \[_, pkh] -> do
    let pkhs = pure $ unPaymentPubKeyHash pkh

    void $ waitNSlots 50

    cnft <- generateNft
    void $ waitNSlots 1

    nft1 <- mintWithCollection (cnft, MintParams (toEnum 0) (toEnum 50_00) (toEnum 10_000_000) 5 5 Nothing pkhs Nothing "V1")
    void $ waitNSlots 1

    nft2 <- setPrice (SetPriceParams nft1 (toEnum 50_000_000))
    void $ waitNSlots 1

    nft3 <- marketplaceDeposit nft2
    void $ waitNSlots 1

    pure (nft3, pkhs)

  void $
    withContractAs 1 $
      const $ do
        Contract.logError @String "Before buy"
        nft4 <- marketplaceBuy nft3
        void $ waitNSlots 1

        Contract.logError @String "Before mp set price"
        nft5 <- marketplaceSetPrice (SetPriceParams nft4 (toEnum 25_000_000))
        void $ waitNSlots 1

        Contract.logError @String "Before redeem"
        nft6 <- marketplaceRedeem nft5
        void $ waitNSlots 1

        Contract.logError @String "Before set price"
        nft7 <- setPrice (SetPriceParams nft6 (toEnum 20_000_000))
        void $ waitNSlots 1

        Contract.logError @String "Before burn"
        burn nft7
        void $ waitNSlots 1

  withContractAs 2 $
    const $ do
      feeWithdraw pkhs
      void $ waitNSlots 1

testGift :: TestCase
testGift = do
  (outcomeResult -> (Right nft2)) <- withContractAs 0 $ \[_, pkh] -> do
    let pkhs = pure $ unPaymentPubKeyHash pkh
    cnft <- generateNft
    void $ waitNSlots 1

    nft1 <- mintWithCollection (cnft, MintParams (toEnum 0) (toEnum 50_00) (toEnum 0) 5 5 Nothing pkhs Nothing "V1")
    void $ waitNSlots 3

    nft2 <- marketplaceDeposit nft1
    void $ waitNSlots 1

    pure nft2

  withContractAs 1 $
    const $ do
      void $ marketplaceBuy nft2
      void $ waitNSlots 1

testChangePriceNotOwner :: TestCase
testChangePriceNotOwner = do
  (outcomeResult -> (Right nft2)) <- withContractAs 0 $
    const $ do
      cnft <- generateNft
      void $ waitNSlots 1

      nft1 <- mintWithCollection (cnft, MintParams (toEnum 0) (toEnum 0) (toEnum 10_000_000) 5 5 Nothing [] Nothing "V1")
      void $ waitNSlots 1

      nft2 <- marketplaceDeposit nft1
      void $ waitNSlots 1

      pure nft2

  withContractAs 1 $
    const $ do
      void $ marketplaceSetPrice (SetPriceParams nft2 (toEnum 20_000_000))
      void $ waitNSlots 1

testRedeemNotOwner :: TestCase
testRedeemNotOwner = do
  (outcomeResult -> Right nft2) <- withContractAs 0 $
    const $ do
      cnft <- generateNft
      void $ waitNSlots 1

      nft1 <- mintWithCollection (cnft, MintParams (toEnum 0) (toEnum 0) (toEnum 10_000_000) 5 5 Nothing [] Nothing "V1")
      void $ waitNSlots 1

      nft2 <- marketplaceDeposit nft1
      void $ waitNSlots 1

      pure nft2

  withContractAs 1 $
    const $ do
      void $ marketplaceRedeem nft2
      void $ waitNSlots 1

testBurnTooEarly :: TestCase
testBurnTooEarly = do
  withContractAs 0 $
    const $ do
      cnft <- generateNft
      void $ waitNSlots 1

      nft1 <- mintWithCollection (cnft, MintParams (toEnum 0) (toEnum 0) (toEnum 10_000_000) 5_000_000_000 5_000_000_000 Nothing [] Nothing "V1")
      void $ waitNSlots 1

      burn nft1
