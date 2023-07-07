import { Router, Request, Response } from 'express';
import { adminProfile } from '../controller/profile.controller';
import { middlewareRoleManager } from '../utility/helper/helper';
import upload from '../utility/helper/upload.helper';


const ProfileRouter: Router = Router();

// // Define a route for user registration
ProfileRouter.post('/admin',middlewareRoleManager(["admin"]),upload.single('image'), adminProfile);


export default ProfileRouter;