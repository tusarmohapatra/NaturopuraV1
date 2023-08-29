{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FarmerFertilizerContract where

import PlutusTx.Prelude hiding (Semigroup (..), check)
import Plutus.V1.Ledger.Api hiding (check)
import qualified Plutus.V1.Ledger.Value as Value

data FarmerFertilizerContract
instance Scripts.ValidatorTypes FarmerFertilizerContract where
    type instance RedeemerType FarmerFertilizerContract = ()
    type instance DatumType FarmerFertilizerContract = ()

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ ctx =
    let info = scriptContextTxInfo ctx
        txOut = case findOwnOutput ctx of
            Nothing -> traceError "Expected to find an output with own signature"
            Just o -> o
        ownPubKeyHash = txOutAddress (txOutTxOut txOut)
        inputValue = Value.valueOf (txOutValue $ txOutTxOut txOut) ownCurrencySymbol ownTokenName
        requiredValue = Value.singleton ownCurrencySymbol ownTokenName 1
    in inputValue == requiredValue && ownPubKeyHash == fertilizerCompanyPubKeyHash
  where
    ownCurrencySymbol = 
    ownTokenName = 
    fertilizerCompanyPubKeyHash = 

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
