{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Plutip (test) where

import Control.Monad.Reader (ReaderT, liftIO)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Last)
import Data.Text (Text)
import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash), Value)
import Plutus.Contract (waitNSlots)
import Test.Plutip.Contract (assertExecution, initAda, withContractAs)
import Test.Plutip.Internal.Types (ClusterEnv, ExecutionResult (ExecutionResult))
import Test.Plutip.LocalCluster (BpiWallet, withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)
import Prelude

import Control.Monad (void)
import SeabugOnchain.Contract.Burn (burn)
import SeabugOnchain.Contract.FeeWithdraw (feeWithdraw)
import SeabugOnchain.Contract.MarketplaceBuy (marketplaceBuy)
import SeabugOnchain.Contract.MarketplaceDeposit (marketplaceDeposit)
import SeabugOnchain.Contract.MarketplaceRedeem (marketplaceRedeem)
import SeabugOnchain.Contract.MarketplaceSetPrice (marketplaceSetPrice)
import SeabugOnchain.Contract.Mint (generateNft, mintWithCollection)
import SeabugOnchain.Contract.SetPrice (setPrice)
import SeabugOnchain.Types (MintParams (MintParams), NftData, SetPriceParams (SetPriceParams))

-- TODO: Partial value asserts here when added (https://github.com/mlabs-haskell/plutip/issues/42)
test :: TestTree
test =
  withCluster
    "Integration tests"
    [ assertExecution "Happy path" (initAda 100 <> initAda 100 <> initAda 100) testValid [shouldSucceed]
    , assertExecution "Gift path" (initAda 100 <> initAda 100) testGift [shouldSucceed]
    , assertExecution "Fail to change price when not owner" (initAda 100 <> initAda 100) testChangePriceNotOwner [shouldFail]
    , assertExecution "Fail to redeem when not owner" (initAda 100 <> initAda 100) testRedeemNotOwner [shouldFail]
    , assertExecution "Fail unlocking too early" (initAda 100) testBurnTooEarly [shouldFail]
    ]

type TestCase = ReaderT (ClusterEnv, NonEmpty BpiWallet) IO (ExecutionResult (Last NftData) Text ((), NonEmpty Value))

testValid :: TestCase
testValid = do
  (ExecutionResult res _) <- withContractAs 0 $ \[_, pkh] -> do
    let pkhs = pure $ unPaymentPubKeyHash pkh
    cnft <- generateNft
    void $ waitNSlots 1

    nft1 <- mintWithCollection (cnft, MintParams 0 50_00 10_000_000 5 5 Nothing pkhs Nothing "V1")
    void $ waitNSlots 1

    nft2 <- setPrice (SetPriceParams nft1 50_000_000)
    void $ waitNSlots 1

    nft3 <- marketplaceDeposit nft2
    void $ waitNSlots 1

    pure (nft3, pkhs)

  liftIO $ print res
  (Right ((nft3, pkhs), _)) <- pure res

  void $
    withContractAs 1 $
      const $ do
        nft4 <- marketplaceBuy nft3
        void $ waitNSlots 1

        nft5 <- marketplaceSetPrice (SetPriceParams nft4 25_000_000)
        void $ waitNSlots 1

        nft6 <- marketplaceRedeem nft5
        void $ waitNSlots 1

        nft7 <- setPrice (SetPriceParams nft6 20_000_000)
        void $ waitNSlots 1

        burn nft7
        void $ waitNSlots 1

  withContractAs 2 $
    const $ do
      feeWithdraw pkhs
      void $ waitNSlots 1

testGift :: TestCase
testGift = do
  (ExecutionResult (Right (nft2, _)) _) <- withContractAs 0 $ \[_, pkh] -> do
    let pkhs = pure $ unPaymentPubKeyHash pkh
    cnft <- generateNft
    void $ waitNSlots 1

    nft1 <- mintWithCollection (cnft, MintParams 0 50_00 0 5 5 Nothing pkhs Nothing "V1")
    void $ waitNSlots 1

    nft2 <- marketplaceDeposit nft1
    void $ waitNSlots 1

    pure nft2

  withContractAs 1 $
    const $ do
      void $ marketplaceBuy nft2
      void $ waitNSlots 1

testChangePriceNotOwner :: TestCase
testChangePriceNotOwner = do
  (ExecutionResult (Right (nft2, _)) _) <- withContractAs 0 $
    const $ do
      cnft <- generateNft
      void $ waitNSlots 1

      nft1 <- mintWithCollection (cnft, MintParams 0 0 10_000_000 5 5 Nothing [] Nothing "V1")
      void $ waitNSlots 1

      nft2 <- marketplaceDeposit nft1
      void $ waitNSlots 1

      pure nft2

  withContractAs 1 $
    const $ do
      void $ marketplaceSetPrice (SetPriceParams nft2 20_000_000)
      void $ waitNSlots 1

testRedeemNotOwner :: TestCase
testRedeemNotOwner = do
  (ExecutionResult (Right (nft2, _)) _) <- withContractAs 0 $
    const $ do
      cnft <- generateNft
      void $ waitNSlots 1

      nft1 <- mintWithCollection (cnft, MintParams 0 0 10_000_000 5 5 Nothing [] Nothing "V1")
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

      nft1 <- mintWithCollection (cnft, MintParams 0 0 10_000_000 5_000_000_000 5_000_000_000 Nothing [] Nothing "V1")
      void $ waitNSlots 1

      burn nft1
