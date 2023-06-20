import { Request, Response } from "express";
let Validator = require("validatorjs");
const Joi = require("joi");
const bcrypt = require("bcryptjs");
const saltRounds = 10;
import User from "../model/user.model";
import env from "../environment/environment";
const jwt = require("jsonwebtoken");

export const userSignup = async (req: Request, res: Response) => {
  try {
    const { firstName, lastName, email, password, isRemember } = req.body;

    const schema = Joi.object({
      firstName: Joi.string().min(3).max(30).required(),
      lastName: Joi.string().min(3).max(30).required(),

      password: Joi.string()
        .pattern(new RegExp(/^(?=.*\d)(?=.*[a-z])(?=.*[A-Z]).{6,20}$/))
        .required(),

      isRemember: Joi.boolean().required(),

      email: Joi.string()
        .email({ minDomainSegments: 2, tlds: { allow: ["com", "net"] } })
        .required(),
    });

    const { error, value } = schema.validate(req.body);

    if (error) {
      return res.status(400).json({ error: error.details });
    } else {
      const user = await User.findOne({
        where: { email: email, deletedAt: null },
      });

      if (!user) {
        return res.status(400).json({
          error: { messages: "you have already signup try to login.",status:'error' },
        });
      } else {
        let hashPass: string = "";
        bcrypt
          .genSalt(saltRounds)
          .then((salt: string) => {
            return bcrypt.hash(password, salt);
          })
          .then(async (hash: string) => {
            const customer = await User.create({
              firstName: firstName,
              lastName: lastName,
              role: "consumer",
              email: email,
              password: hash,
            });
            const newCustomer = {
              isActive: customer.isActive,
              id: customer.id,
              firstName: customer.firstName,
              lastName: customer.lastName,
              role: customer.role,
              email: customer.email,
            };
            return res.status(201).json({
              message: "successfully register.",
              status: "success",
              token: isRemember
                ? jwt.sign(newCustomer, env.TOKEN_SECRET, { expiresIn: "48h" })
                : "",
            });
          })
          .catch((err: any) => console.error(err.message));
      }
    }
  } catch (error) {
    return res.status(500).json({ message: "An internal server error occurred",status:'error' });
  }
};



export const userLogin = async (req: Request, res: Response) => {
  const { email, password } = req.body;

}