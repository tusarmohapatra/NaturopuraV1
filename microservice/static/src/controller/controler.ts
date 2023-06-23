import { Request, Response } from "express";
import connectToDatabase from "../database/database";
import { createErrorResponse, createSuccessResponse } from "../utility/helper/helper";

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
