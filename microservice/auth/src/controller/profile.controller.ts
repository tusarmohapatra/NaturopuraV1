import { Request, Response } from "express";
import {
  createErrorResponse,
  createSuccessResponse,
  getPayloadFromToken,
} from "../utility/helper/helper";
import User from "../model/user.model";
const Joi = require("joi");

export const adminProfile = async (req: any, res: Response) => {
  const { firstName, lastName, email } = req.body;

  const schema = Joi.object({
    firstName: Joi.string().required(),
    lastName: Joi.string().min(3).max(30),
    email: Joi.string().email({ minDomainSegments: 2, tlds: { allow: ["com", "net"] } })
    .required(),
  });

  const { error, value } = schema.validate(req.body);

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
      await User.update(
        {
          firstName,
          lastName,
          email,
          updatedBy: payload?.id,
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
