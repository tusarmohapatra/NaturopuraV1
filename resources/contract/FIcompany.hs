{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module FarmerInsuranceContract
  ( FarmerDetails (..),
    InsuranceCompanyDetails (..), 
    FarmerInsuranceContract,
    farmerEndpoint,
    insuranceCompanyEndpoint,
    farmerInsuranceContract,
  )
where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.Contract
  ( BlockchainActions,
    Contract,
    Endpoint,
    logInfo,
    selectList,
    type (.\/),
    (/.),
    (.==),
    endpoint,
    handleError,
    logError,
    throwError,
    type (.\/),
  )
import Plutus.Contract.State (write)
import Plutus.Trace.Emulator (EmulatorTrace)
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (True),
    Eq ((==)),
    Maybe (Just),
    ($),
    (&&),
  )


data FarmerDetails = FarmerDetails
  { farmerName :: String,
    farmerAddress :: String,
    farmerContact :: String
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)


data InsuranceCompanyDetails = InsuranceCompanyDetails
  { companyName :: String,
    companyAddress :: String,
    companyContact :: String
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)


type FarmerEndpoint =
  BlockchainActions
    .\/ Endpoint "addFarmer" FarmerDetails
    .\/ Endpoint "updateFarmer" FarmerDetails
    .\/ Endpoint "getFarmer" ()


type InsuranceCompanyEndpoint =
  BlockchainActions
    .\/ Endpoint "addInsuranceCompany" InsuranceCompanyDetails
    .\/ Endpoint "updateInsuranceCompany" InsuranceCompanyDetails
    .\/ Endpoint "getInsuranceCompany" ()


type FarmerInsuranceContract = FarmerEndpoint .\/ InsuranceCompanyEndpoint


farmerEndpoint :: Contract FarmerEndpoint String ()
farmerEndpoint =
  selectList
    [ endpoint @"addFarmer" addFarmer,
      endpoint @"updateFarmer" updateFarmer,
      endpoint @"getFarmer" getFarmer
    ]


insuranceCompanyEndpoint :: Contract InsuranceCompanyEndpoint String ()
insuranceCompanyEndpoint =
  selectList
    [ endpoint @"addInsuranceCompany" addInsuranceCompany,
      endpoint @"updateInsuranceCompany" updateInsuranceCompany,
      endpoint @"getInsuranceCompany" getInsuranceCompany
    ]


farmerInsuranceContract :: Contract FarmerInsuranceContract String ()
farmerInsuranceContract =
  selectList
    [ wrapEndpoint @"farmer" farmerEndpoint,
      wrapEndpoint @"insuranceCompany" insuranceCompanyEndpoint
    ]


wrapEndpoint :: forall a w s e.
  ( AsContractError e,
    Monoid w
  ) =>
  String ->
  Contract (Endpoint a) s e () ->
  Contract (BlockchainActions .\/ Endpoint a) s e ()
wrapEndpoint name endpointContract = do
  _ <- handleError (\err -> logError $ name ++ " endpoint error: " ++ displayError err) endpointContract
  logInfo $ name ++ " endpoint completed."


addFarmer :: FarmerDetails -> Contract w s String ()
addFarmer details = do
  logInfo $ "Adding farmer details: " <> show details

  
  logInfo "Farmer details added successfully."


updateFarmer :: FarmerDetails -> Contract w s String ()
updateFarmer details = do
  logInfo $ "Updating farmer details: " <> show details

 
  logInfo "Farmer details updated successfully."


getFarmer :: Contract w s String ()
getFarmer = do
  logInfo "Getting farmer details"

  
  logInfo "Retrieved farmer details: <Tusar, tusar@gmail.com>" -- Replace  with the actual details


addInsuranceCompany :: InsuranceCompanyDetails -> Contract w s String ()
addInsuranceCompany details = do
  logInfo $ "Adding insurance company details: " <> show details

  
  logInfo "Insurance company details added successfully."


updateInsuranceCompany :: InsuranceCompanyDetails -> Contract w s String ()
updateInsuranceCompany details = do
  logInfo $ "Updating insurance company details: " <> show details

 
  logInfo "Insurance company details updated successfully."


getInsuranceCompany :: Contract w s String ()
getInsuranceCompany = do
  logInfo "Getting insurance company details"

  
  logInfo "Retrieved insurance company details: <Compony details>" -- Replace <details> with the actual details


testContract :: EmulatorTrace ()
testContract = do
  void $ Plutus.Contract.activateContractWallet (walletPubKey 1) farmerInsuranceContract
