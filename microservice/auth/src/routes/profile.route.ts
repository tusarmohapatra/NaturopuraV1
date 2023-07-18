import { Router, Request, Response } from 'express';
import { adminProfile, farmerProfile, userProfile } from '../controller/profile.controller';
import { middlewareRoleManager } from '../utility/helper/helper';
import upload from '../utility/helper/upload.helper';


const ProfileRouter: Router = Router();

// // Define a route for user registration
ProfileRouter.post('/admin',middlewareRoleManager(["admin"]),upload.single('image'), adminProfile);
ProfileRouter.post('/user',middlewareRoleManager(["consumer"]),upload.single('image'), userProfile);
ProfileRouter.post('/farmer',middlewareRoleManager(["farmer"]),upload.single('image'), farmerProfile);



export default ProfileRouter;