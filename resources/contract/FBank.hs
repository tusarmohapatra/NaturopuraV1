{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FarmerBankContract where

import PlutusTx.Prelude hiding (Semigroup (..), check)
import Plutus.V1.Ledger.Api hiding (check)
import qualified PlutusTx


data FarmerBankContract
instance Scripts.ValidatorTypes FarmerBankContract where
    type instance RedeemerType FarmerBankContract = ()
    type instance DatumType FarmerBankContract = Integer


{-# INLINEABLE mkValidator #-}
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ loanAmount ctx =
    let info = scriptContextTxInfo ctx
        ownPubKeyHash = txInfoOwnPubKeyHash info
        inputValue = valueLockedBy info (scriptAddress ctx)
        requiredValue = Ada.lovelaceValueOf loanAmount
    in inputValue == requiredValue && ownPubKeyHash == bankPubKeyHash
  where
    bankPubKeyHash = 


validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])


valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
