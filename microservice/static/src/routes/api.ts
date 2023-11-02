import { Router, Request, Response } from 'express';
import {getChemicalCategory, getCountryCode, getEquipmentCategory, getProductCategory } from "../controller/controller";

const router: Router = Router();

// Define a route for user registration
router.get('/countryCode', getCountryCode);
router.get('/productCategory', getProductCategory);
router.get('/equipmentCategory', getEquipmentCategory);
router.get('/chemicalCategory', getChemicalCategory);



export default router;