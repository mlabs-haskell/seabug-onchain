module Test.Utils (
  throwError,
  next,
  wait,
  concatPredicates,
  walletFromNumber,
) where

import PlutusTx.Prelude hiding (fromInteger)
import Prelude (String, fromInteger)

import Data.Functor (void)
import Data.List (foldl1')
import Ledger.CardanoWallet (WalletNumber (..))
import Plutus.Contract.Test (TracePredicate, (.&&.))
import Plutus.Trace.Emulator qualified as Trace
import Wallet.Emulator.Wallet (Wallet, fromWalletNumber)

-- | Throws error to emulator trace.
throwError :: String -> Trace.EmulatorTrace a
throwError msg = Trace.throwError (Trace.GenericError $ "Generic Error:" <> msg)

-- | Wait for one slot.
next :: Trace.EmulatorTrace ()
next = void Trace.nextSlot

-- | Wait given amount of slots.
wait :: Integer -> Trace.EmulatorTrace ()
wait = void . Trace.waitNSlots . fromInteger

concatPredicates :: [TracePredicate] -> TracePredicate
concatPredicates = foldl1' (.&&.)

walletFromNumber :: Integer -> Wallet
walletFromNumber = fromWalletNumber . WalletNumber
