import { Request, Response } from "express";
import connectToDatabase from "../database/database";
import { createErrorResponse, createSuccessResponse } from "../utility/helper/helper";
import { Sequelize } from "sequelize";
import productDb from "../database/productDb";

export const getCountryCode = async (req: Request, res: Response) => {
  connectToDatabase()
    .then((db) => {
      const collection = db.collection("countryCode");
      collection
        .find({})
        .toArray()
        .then((result) => {
         return res.status(200).json(createSuccessResponse("Successfully fetch country code.","",result));
        });
    })
    .catch((error: any) => {
        return res
        .status(500)
        .json(createErrorResponse('ERROR_WHILE_GETTING_DATA_FROM_SERVER','Unable to get data from server.',{}));
    });
};




export const getProductCategory = async (req: Request, res: Response) => {

  const [results, metadata] = await productDb.query("SELECT id,name,description,parentCategory,recommendedStorageConditions,shelfLifeIndDays,harvestingSeason FROM `productCategories` WHERE `deletedAt` IS NULL");
  return res.send(results);
};

export const getEquipmentCategory = async (req: Request, res: Response) => {
  const [results, metadata] = await productDb.query("SELECT id,name,description,parentCategory,recommendedUse,keyFeatures,power_source,maintenance_requirements,warranty FROM `equipmentCategories` WHERE `deletedAt` IS NULL");
  return res.send(results);
};

export const getChemicalCategory = async (req: Request, res: Response) => {
  const [results, metadata] = await productDb.query("SELECT id,name,description,parentCategory,recommendedUse,type,activeIngredient,applicationMethod,safetyPrecautions FROM `chemicalCategories` WHERE `deletedAt` IS NULL");
  return res.send(results);
};



	