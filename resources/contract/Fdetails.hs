{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fdetails where

import qualified Data.ByteString.Char8 as BS8
import Plutus.V1.Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    CurrencySymbol,
    MintingPolicy,
    ScriptContext (scriptContextTxInfo),
    TokenName (unTokenName),
    TxId (TxId, getTxId),
    TxInInfo (txInInfoOutRef),
    TxInfo (txInfoInputs, txInfoMint),
    TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
    mkMintingPolicyScript,
  )
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.Prelude
  ( Bool (False),
    Eq ((==)),
    any,
    traceIfFalse,
    ($),
    (&&),
  )
import Text.Printf (printf)
import Utilities
  ( bytesToHex,
    currencySymbol,
    wrapPolicy,
    writeCodeToFile,
    writePolicyToFile,
  )
import Prelude (IO, Show (show), String)

data FarmerDetails = FarmerDetails
  { farmerName :: String,
    farmerAddress :: String,
    farmerContact :: String
  }

type FarmerContract =
  BlockchainActions
    .\/ Endpoint "addFarmer" FarmerDetails
    .\/ Endpoint "updateFarmer" FarmerDetails
    .\/ Endpoint "getFarmer" ()

addFarmer :: FarmerDetails -> Contract w s Text ()
addFarmer details = do
  logInfo @String "Adding farmer details..."



updateFarmer :: FarmerDetails -> Contract w s Text ()
updateFarmer details = do
  logInfo @String "Updating farmer details..."



getFarmer :: Contract w s String ()
getFarmer = do
  logInfo "Getting farmer details"


contract :: Contract FarmerEndpoints String ()
contract = do
  selectList
    [ endpoint @"addFarmer" addFarmer,
      endpoint @"updateFarmer" updateFarmer,
      endpoint @"getFarmer" getFarmer
    ]
