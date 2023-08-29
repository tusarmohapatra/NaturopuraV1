{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ctransaction where

-- import Ledger hiding (singleton)
-- import Ledger.Constraints as Constraints
-- import Playground.Contract
-- import Plutus.Contract
-- import qualified PlutusTx
-- import PlutusTx.Prelude hiding (Semigroup (..), unless)

import Plutus.Contract
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    CurrencySymbol,
    MintingPolicy,
    ScriptContext,
    mkMintingPolicyScript,
  )
import qualified PlutusTx
import PlutusTx.Prelude (Bool (True))
import Utilities
  ( currencySymbol,
    wrapPolicy,
    writePolicyToFile,
  )
import Prelude (IO)

type ContractSchema =
  Endpoint "cancel" ()

{-# INLINEABLE validate #-}
validate :: () -> () -> ScriptContext -> Bool
validate _ _ _ = False

contract :: Contract () ContractSchema ()
contract = do
  _ <- endpoint @"cancel"
  Contract.logInfo @String "Transaction cancelled!"

mkSchemaDefinitions ''ContractSchema
mkKnownCurrencies []

endpoints :: Contract () ContractSchema ()
endpoints = contract

runContract :: IO ()
runContract = runEmulatorTraceIO $ do
  let wallet = Wallet 1
  h <- activateContractWallet wallet endpoints
  callEndpoint @"cancel" h ()
  void $ Emulator.waitNSlots 1