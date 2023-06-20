import { Router, Request, Response } from 'express';
import User from '../model/user.model';
import env from '../environment/environment';
import { userSignup } from "../controller/auth";

const router: Router = Router();

// Define a route for user registration
router.post('/user/signup', userSignup);

export default router;