import { Request, Response, response } from "express";
import {
  checkCategory,
  colorLog,
  createErrorResponse,
  createSuccessResponse,
  customArrayValidator,
  getPayloadFromToken,
} from "../utility/helper/helper";
import User from "../model/user.model";
import { publishUserUpdateEvent } from "../event/event";
import UserMeta from "../model/userMeta.model";
import productDb from "../database/productDb";
const Joi = require("joi");


const personal = {
  firstName: Joi.string().min(3).max(30).required(),
  lastName: Joi.string().min(3).max(30).required(),
  email: Joi.string()
    .email({ minDomainSegments: 2, tlds: { allow: ["com", "net"] } })
    .required(),
};

const address = {
  address_line1: Joi.string().max(100).required(),
  address_line2: Joi.string().max(100),
  city: Joi.string().max(30).required(),
  state: Joi.string().max(30).required(),
  postal_code: Joi.string().min(5).max(10).required(),
  country: Joi.string().max(44).required(),
};


export const adminProfile = async (req: any, res: Response) => {
  const { firstName, lastName, email } = req.body;

  const schema = Joi.object({
    ...personal
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
  const schema = Joi.object({...personal,...address});
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
    productCategories,
    bio
  } = req.body;

 

  const schema = Joi.object({
    ...personal,
    ...address,
    farmSize: Joi.number().max(50).required(),
    farmingExperience: Joi.number().max(50).required(),
    preferredCommunicationMethod: Joi.string().custom(customArrayValidator, 'custom array validation'),
    productsProduce: Joi.string().custom(customArrayValidator, 'custom array validation'),
    productCategories:Joi.string().custom(customArrayValidator, 'custom array validation'),
    productionCapacity: Joi.string().max(60),
    governmentId:Joi.string().max(30).required(),
    farmLocation:Joi.string().required(),
    bio:Joi.string().max(300).required()
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


  checkCategory(productCategories, res, "productCategories");

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
           productCategories,
           bio,
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
        productCategories,
        governmentId,
        farmLocation,
        bio,
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

export const coldStorage = async (req: any, res: Response) => {
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
    preferredCommunicationMethod,
    governmentId,
    location,
    storageCapacity,
    temperatureZones,
    companyName,
    bio
  } = req.body;

  const schema = Joi.object({
    ...personal,
    ...address,
    preferredCommunicationMethod: Joi.string().custom(
      customArrayValidator,
      "custom array validation"
    ),
    governmentId: Joi.string().required(),
    location: Joi.string().required(),
    storageCapacity: Joi.string(),
    temperatureZones: Joi.string(),
    companyName:Joi.string(),
    bio:Joi.string()
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
      const userMeta = await UserMeta.findOne({
        where: { userId: payload?.id },
      });
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
            preferredCommunicationMethod,
            governmentId,
            farmLocation: location,
            storageCapacity,
            temperatureZones,
            companyName,
            bio,
            updatedBy: payload?.id.toString(),
          },
          { where: { userId: payload?.id } }
        );
      } else {
        await UserMeta.create({
          preferredCommunicationMethod,
          governmentId,
          farmLocation: location,
          storageCapacity,
          temperatureZones,
          companyName,
          bio,
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

export const consultants = async (req: any, res: Response) => {
  const {
    firstName,
    lastName,
    email,
    preferredCommunicationMethod,
    governmentId,
    companyName,
    bio
  } = req.body;

  const schema = Joi.object({
    ...personal,
    preferredCommunicationMethod: Joi.string().custom(
      customArrayValidator,
      "custom array validation"
    ),
    governmentId: Joi.string().required(),
    companyName:Joi.string(),
    bio:Joi.string()
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
      const userMeta = await UserMeta.findOne({
        where: { userId: payload?.id },
      });
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
            preferredCommunicationMethod,
            governmentId,
            companyName,
            bio,
            updatedBy: payload?.id.toString(),
          },
          { where: { userId: payload?.id } }
        );
      } else {
        await UserMeta.create({
          preferredCommunicationMethod,
          governmentId,
          companyName,
          bio,
          userId: payload?.id,
        });
      }
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
}

export const pesticideDealers = async (req: any, res: Response) => {
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
    preferredCommunicationMethod,
    governmentId,
    location,
    chemicalCategories,
    bio,
    companyName
  } = req.body;


  const schema = Joi.object({
    ...personal,
    ...address,
    preferredCommunicationMethod: Joi.string().custom(
      customArrayValidator,
      "custom array validation"
    ),
    chemicalCategories: Joi.string().custom(
      customArrayValidator,
      "custom array validation"
    ),
    governmentId: Joi.string().required(),
    location: Joi.string().required(),
    companyName: Joi.string(),
    bio: Joi.string(),
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

  checkCategory(chemicalCategories,res,'chemicalCategories');

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
      const userMeta = await UserMeta.findOne({
        where: { userId: payload?.id },
      });
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
            preferredCommunicationMethod,
            governmentId,
            farmLocation: location,
            companyName,
            bio,
            chemicalCategories,
            updatedBy: payload?.id.toString(),
          },
          { where: { userId: payload?.id } }
        );
      } else {
        await UserMeta.create({
          preferredCommunicationMethod,
          governmentId,
          farmLocation: location,
          companyName,
          chemicalCategories,
          bio,
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

export const equipmentSealers = async (req: any, res: Response) => {
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
    preferredCommunicationMethod,
    governmentId,
    location,
    equipmentCategories,
    bio,
    companyName,
  } = req.body;

  const schema = Joi.object({
    ...personal,
    ...address,
    preferredCommunicationMethod: Joi.string().custom(
      customArrayValidator,
      "custom array validation"
    ),
    equipmentCategories: Joi.string().custom(
      customArrayValidator,
      "custom array validation"
    ),
    governmentId: Joi.string().required(),
    location: Joi.string().required(),
    companyName: Joi.string(),
    bio: Joi.string(),
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

  checkCategory(equipmentCategories, res, "equipmentCategories");

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
      const userMeta = await UserMeta.findOne({
        where: { userId: payload?.id },
      });
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
            preferredCommunicationMethod,
            governmentId,
            farmLocation: location,
            companyName,
            bio,
            equipmentCategories,
            updatedBy: payload?.id.toString(),
          },
          { where: { userId: payload?.id } }
        );
      } else {
        await UserMeta.create({
          preferredCommunicationMethod,
          governmentId,
          farmLocation: location,
          companyName,
          equipmentCategories,
          bio,
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

export const exportAgency = async (req: any, res: Response) => {

  const {
    firstName,lastName,email,address_line1,address_line2,city,state,postal_code,country,preferredCommunicationMethod,location,companyName,bio,governmentId
  } = req.body;

  const schema = Joi.object({
    ...personal,
    ...address,
    preferredCommunicationMethod: Joi.string().custom(
      customArrayValidator,
      "custom array validation"
    ),
    governmentId: Joi.string().required(),
    location: Joi.string().required(),
    companyName: Joi.string(),
    bio: Joi.string(),
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
      const userMeta = await UserMeta.findOne({
        where: { userId: payload?.id },
      });
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
            preferredCommunicationMethod,
            governmentId,
            farmLocation: location,
            companyName,
            bio,
            updatedBy: payload?.id.toString(),
          },
          { where: { userId: payload?.id } }
        );
      } else {
        await UserMeta.create({
          preferredCommunicationMethod,
          governmentId,
          farmLocation: location,
          companyName,
          bio,
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




export const insurance = async (req: any, res: Response) => {
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
    preferredCommunicationMethod,
    location,
    companyName,
    bio,
    governmentId,
  } = req.body;

  const schema = Joi.object({
    ...personal,
    ...address,
    preferredCommunicationMethod: Joi.string().custom(
      customArrayValidator,
      "custom array validation"
    ),
    governmentId: Joi.string().required(),
    location: Joi.string().required(),
    companyName: Joi.string(),
    bio: Joi.string(),
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
      const userMeta = await UserMeta.findOne({
        where: { userId: payload?.id },
      });
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
            preferredCommunicationMethod,
            governmentId,
            farmLocation: location,
            companyName,
            bio,
            updatedBy: payload?.id.toString(),
          },
          { where: { userId: payload?.id } }
        );
      } else {
        await UserMeta.create({
          preferredCommunicationMethod,
          governmentId,
          farmLocation: location,
          companyName,
          bio,
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
