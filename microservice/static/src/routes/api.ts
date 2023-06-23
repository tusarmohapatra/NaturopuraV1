import { Router, Request, Response } from 'express';
import {getCountryCode } from "../controller/controler";

const router: Router = Router();

// Define a route for user registration
router.get('/countryCode', getCountryCode);




export default router;