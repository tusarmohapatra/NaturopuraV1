import { Request, Response } from "express";
import {
  colorLog,
  createErrorResponse,
  createSuccessResponse,
  getPayloadFromToken,
} from "../utility/helper/helper";
import User from "../model/user.model";
import { publishUserUpdateEvent } from "../event/event";
import UserMeta from "../model/userMeta.model";
import productDb from "../database/productDb";
const Joi = require("joi");

export const adminProfile = async (req: any, res: Response) => {
  const { firstName, lastName, email } = req.body;

  const schema = Joi.object({
    firstName: Joi.string().required(),
    lastName: Joi.string().min(3).max(30),
    email: Joi.string()
      .email({ minDomainSegments: 2, tlds: { allow: ["com", "net"] } })
      .required(),
  });

  const { error, value } = schema.validate(req.body);

  const user = await User.findOne({ where: { email: email } });
  if (user) {
    return res
      .status(400)
      .json(
        createErrorResponse(
          "EMAIL_ALREADY_EXIT",
          "Invalid email id provided.",
          {}
        )
      );
  }

  if (error) {
    return res
      .status(400)
      .json(
        createErrorResponse(
          "INVALID_INPUT",
          "Invalid input provided.",
          error.details
        )
      );
  } else {
    try {
      const payload = getPayloadFromToken(req);
      const user = await User.findOne({ where: { id: payload?.id } });
      await User.update(
        {
          firstName,
          lastName,
          email,
          image: req.file?.key ? req.file?.key : user.image,
          updatedBy: payload?.id.toString(),
        },
        { where: { id: payload?.id } }
      );

      return res
        .status(200)
        .json(createSuccessResponse("Profile update successfully.", ""));
    } catch (error) {
      return res
        .status(500)
        .json(
          createErrorResponse(
            "UNABLE_TO_UPDATE_PROFILE",
            "cant able to update the profile",
            {}
          )
        );
    }
  }
};

export const userProfile = async (req: any, res: Response) => {
  const {
    firstName,
    lastName,
    email,
    address_line1,
    address_line2,
    city,
    state,
    postal_code,
    country,
  } = req.body;
  const schema = Joi.object({
    firstName: Joi.string().required(),
    lastName: Joi.string().min(3).max(30),
    email: Joi.string()
      .email({ minDomainSegments: 2, tlds: { allow: ["com", "net"] } })
      .required(),
    address_line1: Joi.string().required(),
    address_line2: Joi.string(),
    city: Joi.string().required(),
    state: Joi.string().required(),
    postal_code: Joi.string().required(),
    country: Joi.string().required(),
  });


  const { error, value } = schema.validate(req.body);
  const user = await User.findOne({ where: { email: email } });
  if (user) {
    return res
      .status(400)
      .json(
        createErrorResponse(
          "EMAIL_ALREADY_EXIT",
          "Invalid email id provided.",
          {}
        )
      );
  }
  if (error) {
    return res
      .status(400)
      .json(
        createErrorResponse(
          "INVALID_INPUT",
          "Invalid input provided.",
          error.details
        )
      );
  } else {
    try {
      const payload = getPayloadFromToken(req);
      const user = await User.findOne({ where: { id: payload?.id } });
      await User.update(
        {
          firstName,
          lastName,
          email,
          image: req.file?.key ? req.file?.key : user.image ,
          updatedBy: payload?.id.toString(),
        },
        { where: { id: payload?.id } }
      );
      publishUserUpdateEvent({
        user_id: payload?.id,
        address_line1,
        address_line2,
        country,
        state,
        city,
        postal_code,
        type: "user",
      });
      return res
        .status(200)
        .json(createSuccessResponse("Profile update successfully.", ""));
    } catch (error) {
      return res
        .status(500)
        .json(
          createErrorResponse(
            "UNABLE_TO_UPDATE_PROFILE",
            "cant able to update the profile",
            {}
          )
        );
    }
  }
};

export const farmerProfile = async (req: any, res: Response) => {
  const {
    firstName,
    lastName,
    email,
    address_line1,
    address_line2,
    city,
    state,
    postal_code,
    country,
    farmSize,
    farmingExperience,
    preferredCommunicationMethod,
    productsProduce,
    productionCapacity,
    governmentId,
    farmLocation,
    productCategories
  } = req.body;

  const customArrayValidator = (value: any, helpers: any) => {
    try {
      const parsedArray = JSON.parse(value);

      if (!Array.isArray(parsedArray)) {
        return helpers.error("any.invalid");
      }

      return parsedArray;
    } catch (error) {
      return helpers.error("any.invalid");
    }
  };

  const schema = Joi.object({
    firstName: Joi.string().required(),
    lastName: Joi.string().min(3).max(30),
    email: Joi.string()
      .email({ minDomainSegments: 2, tlds: { allow: ["com", "net"] } })
      .required(),
    address_line1: Joi.string().required(),
    address_line2: Joi.string(),
    city: Joi.string().required(),
    state: Joi.string().required(),
    postal_code: Joi.string().required(),
    country: Joi.string().required(),
    farmSize: Joi.number().required(),
    farmingExperience: Joi.number().required(),
    preferredCommunicationMethod: Joi.string().custom(customArrayValidator, 'custom array validation'),
    productsProduce: Joi.string().custom(customArrayValidator, 'custom array validation'),
    productCategories:Joi.string().custom(customArrayValidator, 'custom array validation'),
    productionCapacity: Joi.string(),
    governmentId:Joi.string().required(),
    farmLocation:Joi.string().required()
  });


  const user = await User.findOne({ where: { email: email } });
  if (user) {
    return res
      .status(400)
      .json(
        createErrorResponse(
          "EMAIL_ALREADY_EXIT",
          "Invalid email id provided.",
          {}
        )
      );
  }
  const { error, value } = schema.validate(req.body);

  const parsedArray = JSON.parse(productCategories);
  const [results, metadata] = await productDb.query(`SELECT id FROM productCategories WHERE id IN(${parsedArray})`);

  if(parsedArray.length !== results.length){
    return res
    .status(400)
    .json(
      createErrorResponse(
        "INVALID_CATEGORY",
        "invalid category provided.",
        {}
      )
    );
  }

  if (error) {
    return res
      .status(400)
      .json(
        createErrorResponse(
          "INVALID_INPUT",
          "Invalid input provided.",
          error.details
        )
      );
  } else {
    try {
      const payload = getPayloadFromToken(req);
      const userMeta =   await UserMeta.findOne({ where: { userId: payload?.id } });
      const user = await User.findOne({ where: { id: payload?.id } });

      await User.update(
        {
          firstName,
          lastName,
          email,
          image: req.file?.key ? req.file?.key : user.image,
          updatedBy: payload?.id.toString(),
        },
        { where: { id: payload?.id } }
      );
  

     if (userMeta) {
       await userMeta.update(
         {
           farmSize,
           farmingExperience,
           preferredCommunicationMethod,
           productsProduce,
           productionCapacity,
           governmentId,
           farmLocation,
           updatedBy: payload?.id.toString(),
         },
         { where: { userId: payload?.id } }
       );
     } else {
      await UserMeta.create({
        farmSize,
        farmingExperience,
        preferredCommunicationMethod,
        productsProduce,
        productionCapacity,
        governmentId,
        farmLocation,
        userId: payload?.id,
      });
     }
      publishUserUpdateEvent({
        user_id: payload?.id,
        address_line1,
        address_line2,
        country,
        state,
        city,
        postal_code,
        type: "farmer",
      });
      return res
        .status(200)
        .json(createSuccessResponse("Profile update successfully.", ""));
    } catch (error) { 
      return res
        .status(500)
        .json(
          createErrorResponse(
            "UNABLE_TO_UPDATE_PROFILE",
            "cant able to update the profile",
            {}
          )
      );
   }
  }
};
