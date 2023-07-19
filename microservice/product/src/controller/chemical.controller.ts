import { Request, Response } from "express";
import {
  createErrorResponse,
  createSuccessResponse,
  getPayloadFromToken,
} from "../utility/helper/helper";

import chemicalCategory from "../model/chemical.category";

const Joi = require("joi");




export const createChemicalCategory = async (req: Request, res: Response) => {
  const {
    name,
    description,
    recommendedUse,
    type,
    activeIngredient,
    applicationMethod,
    safetyPrecautions,
  } = req.body;

  const schema = Joi.object({
    name: Joi.string().min(3).max(30).required(),
    description: Joi.string().min(3).max(250).required(),
    recommendedUse: Joi.string().required(),
    type: Joi.string()
      .valid("FERTILIZER", "HERBICIDE", "INSECTICIDE", "FUNGICIDE", "OTHER")
      .required(),
    activeIngredient: Joi.string().valid("YES", "NO").required(),
    applicationMethod: Joi.string(),
    safetyPrecautions: Joi.string(),
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
      const category = await chemicalCategory.create({
        name,
        description,
        recommendedUse,
        type,
        activeIngredient,
        applicationMethod,
        safetyPrecautions,
        createdBy: payload.id,
      });

      return res
        .status(201)
        .json(
          createSuccessResponse("equipment category created successfully", "")
        );
    } catch (error) {
      return res
        .status(500)
        .json(
          createErrorResponse(
            "UNABLE_TO_CRETE_CATEGORY",
            "cant able to create the table",
            {error}
          )
        );
    }
  }
};

export const updateChemicalCategory = async (req: Request, res: Response) => {
  const {
    name,
    description,
    recommendedUse,
    type,
    activeIngredient,
    applicationMethod,
    safetyPrecautions,
    id,
  } = req.body;

  const schema = Joi.object({
    name: Joi.string().min(3).max(30),
    description: Joi.string().min(3).max(250),
    recommendedUse: Joi.string(),
    type: Joi.string().valid(
      "FERTILIZER",
      "HERBICIDE",
      "INSECTICIDE",
      "FUNGICIDE",
      "OTHER"
    ),
    activeIngredient: Joi.string().valid("YES", "NO"),
    applicationMethod: Joi.string(),
    safetyPrecautions: Joi.string(),
    id: Joi.number().required(),
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
    const category = await chemicalCategory.findOne({
      where: { id: id, deletedAt: null },
    });
    if (!category) {
      return res
        .status(404)
        .json(createErrorResponse("RECORD_NOT_EXIT", "Record is not exit."));
    }

    try {
      const payload = getPayloadFromToken(req);
      await chemicalCategory.update(
        {
          name,
          description,
          recommendedUse,
          type,
          activeIngredient,
          applicationMethod,
          safetyPrecautions,
          updatedBy:payload?.id
        },
        { where: { id: id } }
      );
      return res
        .status(200)
        .json(
          createSuccessResponse("Chemical category update successfully.", "")
        );
    } catch (error) {
      return res
        .status(500)
        .json(
          createErrorResponse(
            "UNABLE_TO_UPDATE_CATEGORY",
            "Cant able to update the table.",
            {}
          )
        );
    }
  }
};

export const deleteChemicalCatCategory = async (req: Request, res: Response) => {
  const categoryId = req.params.id;

  const category = await chemicalCategory.findOne({
    where: { id: categoryId, deletedAt: null },
  });

  if (!category) {
    return res
      .status(404)
      .json(createErrorResponse("RECORD_NOT_EXIT", "Record is not exit."));
  } else {
    try {
      const payload = getPayloadFromToken(req);
      await chemicalCategory.update(
        { deletedAt: new Date() },
        { where: { id: categoryId, deletedAt: null, updatedBy:payload?.id } }
      );
      return res
        .status(200)
        .json(
          createSuccessResponse("Chemical category deleted successfully", "")
        );
    } catch (error) {
      return res
        .status(500)
        .json(
          createErrorResponse(
            "UNABLE_TO_DELETE_CATEGORY",
            "cant able to delete the table",
            {}
          )
        );
    }
  }
};