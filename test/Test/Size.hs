module Test.Size (test) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx qualified
import PlutusTx.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Script.Size (fitsOnChain)

import SeabugOnchain.Dao qualified as Dao
import SeabugOnchain.Lock qualified as Lock
import SeabugOnchain.Marketplace qualified as Marketplace
import SeabugOnchain.Token (mkPolicy)

test :: TestTree
test =
  testGroup
    "Size"
    [ testMintingPolicyFitOnChain
    , testLockScriptFitOnChain
    , testMarketplaceScriptFitOnChain
    , testDaoScriptFitOnChain
    ]

testMintingPolicyFitOnChain :: TestTree
testMintingPolicyFitOnChain =
  fitsOnChain "Minting policy" $
    fromCompiledCode $$(PlutusTx.compile [||mkPolicy||])

testLockScriptFitOnChain :: TestTree
testLockScriptFitOnChain =
  fitsOnChain "Lock script" $
    fromCompiledCode $$(PlutusTx.compile [||Lock.mkValidator||])

testMarketplaceScriptFitOnChain :: TestTree
testMarketplaceScriptFitOnChain =
  fitsOnChain "Marketplace script" $
    fromCompiledCode $$(PlutusTx.compile [||Marketplace.mkValidator||])

testDaoScriptFitOnChain :: TestTree
testDaoScriptFitOnChain =
  fitsOnChain "Dao script" $
    fromCompiledCode $$(PlutusTx.compile [||Dao.mkValidator||])
