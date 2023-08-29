{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FarmerBuyerContract where

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


data Crop = Crop
    { cropName :: String,
      cropQuantity :: Integer,
      cropPrice :: Integer
    }

data Farmer = Farmer
    { farmerName :: String,
      farmerCrops :: [Crop]
    }

data Buyer = Buyer
    { buyerName :: String,
      buyerBalance :: Integer
    }


offerCrops :: Farmer -> [Crop] -> Farmer
offerCrops farmer newCrops = farmer { farmerCrops = newCrops }


purchaseCrops :: Buyer -> Farmer -> String -> Integer -> (Buyer, Farmer)
purchaseCrops buyer farmer cropName quantity =
    if cropAvailable cropName farmer && buyerHasBalance buyer totalPrice
        then (updatedBuyer, updatedFarmer)
        else (buyer, farmer)
  where
    crop = findCrop cropName (farmerCrops farmer)
    totalPrice = cropPrice crop * quantity
    updatedBuyer = buyer { buyerBalance = buyerBalance buyer - totalPrice }
    updatedFarmer = farmer { farmerCrops = updateCrops cropName quantity (farmerCrops farmer) }



cropAvailable :: String -> Farmer -> Bool
cropAvailable name farmer = any (\crop -> cropName crop == name) (farmerCrops farmer)

buyerHasBalance :: Buyer -> Integer -> Bool
buyerHasBalance buyer amount = buyerBalance buyer >= amount

findCrop :: String -> [Crop] -> Crop
findCrop name crops = head $ filter (\crop -> cropName crop == name) crops

updateCrops :: String -> Integer -> [Crop] -> [Crop]
updateCrops name quantity = map updateCrop
  where
    updateCrop crop
        | cropName crop == name = crop { cropQuantity = cropQuantity crop - quantity }
        | otherwise = crop
