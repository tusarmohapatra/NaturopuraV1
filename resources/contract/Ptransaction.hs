{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ptransaction where

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

{-# INLINEABLE mkPtransactionPolicy #-}
mkPtransactionPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPtransactionPolicy oref () () ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(_, (), amt)] -> () == () && amt == 1
      _ -> False

mkWrappedPtransactionPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()

mkWrappedPtransactionPolicy tid ix () = wrapPolicy $ mkNFTPolicy oref ()
where
    oref = TxOutRef (TxId $ BuiltinByteString $ BS8.pack tid) (fromIntegral ix)

    oref :: TxOutRef
    oref = TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData tid)
        (PlutusTx.unsafeFromBuiltinData ix)

    tn :: TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'
