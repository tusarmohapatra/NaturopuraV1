import { Request, Response } from "express";
let Validator = require("validatorjs");
const Joi = require("joi");
const bcrypt = require("bcryptjs");
const saltRounds = 10;
import User from "../model/user.model";
import env from "../environment/environment";
import { createErrorResponse,createSuccessResponse } from "../utility/helper/helper";
import { publishUserRegisteredEvent } from "../event/event";
const jwt = require("jsonwebtoken");

export const userSignup = async (req: Request, res: Response) => {
  try {
    const { firstName, lastName, email, signature,key,address, isRemember } = req.body;


    const schema = Joi.object({
      firstName: Joi.string().min(3).max(30).required(),
      lastName: Joi.string().min(3).max(30).required(),
      signature: Joi.string().required(),
      key: Joi.string().required(),
      address: Joi.string().required(),
      isRemember: Joi.boolean().required(),
      email: Joi.string()
        .email({ minDomainSegments: 2, tlds: { allow: ["com", "net"] } })
        .required(),
    });

    const { error, value } = schema.validate(req.body);

    if (error) {
      return res.status(400).json(createErrorResponse('INVALID_INPUT','Invalid input provided.',error.details));
    } else {
      const user = await User.findOne({
        where: { key: key, deletedAt: null },
      });

      if (user) {
        return res.status(400).json(createErrorResponse('USER_ALREADY_EXIT','you have already signup try to login.',{}));
      } else {
        let hashPass: string = "";
        bcrypt
          .genSalt(saltRounds)
          .then((salt: string) => {
            return bcrypt.hash(signature, salt);
          })
          .then(async (hash: string) => {
            const customer = await User.create({
              firstName: firstName,
              lastName: lastName,
              role: "consumer",
              email: email,
              signature: hash,
              key:key,
              walletAddress:address
            });
            const newCustomer = {
              isActive: customer.isActive,
              id: customer.id,
              firstName: customer.firstName,
              lastName: customer.lastName,
              role: customer.role,
              email: customer.email,
            };
            return res.status(201).json(createSuccessResponse("successfully register.",isRemember
            ? jwt.sign(newCustomer, env.TOKEN_SECRET, { expiresIn: "48h" })
            : "",{}));
            
          })
          .catch((err: any) => console.error(err.message));
      }
    }
  } catch (error) {
      return res
        .status(500)
        .json(createErrorResponse('INTERNAL_SERVER_ERROR','An internal server error occurred',{}));
  }
};

export const userLogin = async (req: Request, res: Response) => {
  const { signature,key } = req.body;

  try {
    const schema = Joi.object({
      signature: Joi.string().required(),
      key: Joi.string().required(),
    });

    const { error, value } = schema.validate(req.body);

    if (error) {
      return res.status(400).json(createErrorResponse('INVALID_INPUT','Invalid input provided.',error.details)); 
    } else {
      const user = await User.findOne({
        where: { key: key, deletedAt: null },
      });

      if (!user) {
        return res
          .status(400)
          .json(
            createErrorResponse("USER_NOT_EXIT", "please signup first", {})
          );
      }

      bcrypt.compare(signature, user.signature).then((resData: boolean) => {
        if (resData) {
          const newCustomer = {
            isActive: user.isActive,
            id: user.id,
            firstName: user.firstName,
            lastName: user.lastName,
            role: user.role,
            email: user.email,
          };
          return res.status(201).json(createSuccessResponse("successfully login.",jwt.sign(newCustomer, env.TOKEN_SECRET, {
            expiresIn: "48h",
          }),{}));
        } else {
         
          return res.status(400).json(createErrorResponse("SIGNATURE_NOT_MATCH", "Signature is not match.", {}));
        }
      });
    }
  } catch (error) {
    return res
    .status(500)
    .json(createErrorResponse('INTERNAL_SERVER_ERROR','An internal server error occurred',{}));
  }
};


export const adminLogin = async (req: Request, res: Response) => {
  const { signature,key } = req.body;

  try {
    const schema = Joi.object({
      signature: Joi.string().required(),
      key: Joi.string().required(),
    });

    const { error, value } = schema.validate(req.body);

    if (error) {
      return res.status(400).json(createErrorResponse('INVALID_INPUT','Invalid input provided.',error.details)); 
    } else {
      const user = await User.findOne({
        where: { key: key, deletedAt: null },
      });

      if (!user) {
        return res
          .status(400)
          .json(
            createErrorResponse("USER_NOT_EXIT", "please signup first", {})
          );
      }
    
     if(user.role == 'consumer'){
  
      return res.status(401).json(createErrorResponse("USER_NOT_AUTHORIZE", "you are not authorize for this endpoint.", {}));
     }

      bcrypt.compare(signature, user.signature).then((resData: boolean) => {
        if (resData) {
          const newCustomer = {
            isActive: user.isActive,
            id: user.id,
            firstName: user.firstName,
            lastName: user.lastName,
            role: user.role,
            email: user.email,
          };
          return res.status(201).json(createSuccessResponse("successfully login.",jwt.sign(newCustomer, env.TOKEN_SECRET, {
            expiresIn: "48h",
          }),newCustomer));
        } else {
          return res.status(400).json(createErrorResponse("SIGNATURE_NOT_MATCH", "Signature is not match.", {}));
        }
      });
    }
  } catch (error) {
    return res
    .status(500)
    .json(createErrorResponse('INTERNAL_SERVER_ERROR','An internal server error occurred',{}));
  }
};


export const adminSignup = async (req: Request, res: Response) => {
  try {
    const {
      firstName,
      lastName,
      email,
      isRemember,
      signature,
      key,
      address,
      dialingCode,
      phone,
      addressLine1,
      country,
      state,
      city,
      zipCode,
      type,
    } = req.body;

    const schema = Joi.object({
      firstName: Joi.string().min(3).max(30).required(),
      lastName: Joi.string().min(3).max(30).required(),
      signature: Joi.string().required(),
      key: Joi.string().required(),
      isRemember: Joi.boolean().required(),
      email: Joi.string()
        .email({ minDomainSegments: 2, tlds: { allow: ["com", "net"] } })
        .required(),
      address: Joi.string().required(),
      dialingCode: Joi.string().min(3).required(),
      phone: Joi.number().required(),
      addressLine1: Joi.string().required(),
      country: Joi.string().required(),
      state: Joi.string().required(),
      city: Joi.string().required(),
      zipCode: Joi.string().required(),
      type: Joi.string()
        .valid(
          "admin",
          "farmer",
          "distributors",
          "consultant",
          "agricultural_chemicals",
          "equipment_manufacturers",
          "marketing_agencies",
          "insurance"
        )
        .required(),
    });

    const { error, value } = schema.validate(req.body);

    if (error) {
      return res.status(400).json(createErrorResponse('INVALID_INPUT','Invalid input provided.',error.details));
    } else {
      const user = await User.findOne({
        where: { email: email, deletedAt: null },
      });

      if (user) {
        return res.status(400).json({
          error: {
            messages: "you have already signup try to login.",
            status: "error",
          },
        });
      } else {
        let hashPass: string = "";
        bcrypt
          .genSalt(saltRounds)
          .then((salt: string) => {
            return bcrypt.hash(signature, salt);
          })
          .then(async (hash: string) => {
            const customer = await User.create({
              firstName: firstName,
              lastName: lastName,
              role: type,
              email: email,
              signature: hash,
              key: key,
              walletAddress: address,
            });
            publishUserRegisteredEvent({user_id:customer.id,dialingCode,phone,addressLine1,country,state,city,zipCode});

            const newCustomer = {
              isActive: customer.isActive,
              id: customer.id,
              firstName: customer.firstName,
              lastName: customer.lastName,
              role: customer.role,
              email: customer.email,
            };
            return res.status(201).json(createSuccessResponse("successfully register.",isRemember
            ? jwt.sign(newCustomer, env.TOKEN_SECRET, { expiresIn: "48h" })
            : "",newCustomer));
          })
          .catch((err: any) => console.error(err.message));
      }
    }
  } catch (error) {
    return res
    .status(500)
    .json(createErrorResponse('INTERNAL_SERVER_ERROR','An internal server error occurred',{}));
  }
};