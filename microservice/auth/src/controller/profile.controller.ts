
import { Request, Response } from "express";

const Multer = require('multer'); 

export const adminProfile = async (req: any, res: Response) => {

   return res.send(req?.file.key);
}

