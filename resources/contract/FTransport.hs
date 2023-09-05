{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FarmerTrasportContract where

import PlutusTx.Prelude hiding (Semigroup (..), check)
import Plutus.V1.Ledger.Api hiding (check)
import qualified Plutus.V1.Ledger.Value as Value

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

data TransporterDetails = TransporterDetails
  { transporterName :: String,
    transporterAddress :: String,
    transporterContact :: String
  }
 
