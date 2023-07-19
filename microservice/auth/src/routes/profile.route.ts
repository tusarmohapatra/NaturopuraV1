import { Router, Request, Response } from 'express';
import { adminProfile, coldStorage, consultants, equipmentSealers, exportAgency, farmerProfile, insurance, pesticideDealers, userProfile } from '../controller/profile.controller';
import { middlewareRoleManager } from '../utility/helper/helper';
import upload from '../utility/helper/upload.helper';


const ProfileRouter: Router = Router();

// // Define a route for user registration
ProfileRouter.post('/admin',middlewareRoleManager(["admin"]),upload.single('image'), adminProfile);
ProfileRouter.post('/user',middlewareRoleManager(["consumer"]),upload.single('image'), userProfile);
ProfileRouter.post('/farmer',middlewareRoleManager(["farmer"]),upload.single('image'), farmerProfile);

ProfileRouter.post('/cold-storage',middlewareRoleManager(["cold-storage"]),upload.single('image'), coldStorage);
ProfileRouter.post('/consultants',middlewareRoleManager(["consultant"]),upload.single('image'), consultants);
ProfileRouter.post('/pesticide-dealers',middlewareRoleManager(["agricultural_chemicals"]),upload.single('image'), pesticideDealers);

ProfileRouter.post('/equipment-sealers',middlewareRoleManager(["equipment_manufacturers"]),upload.single('image'), equipmentSealers);
ProfileRouter.post('/export-agency',middlewareRoleManager(["marketing_agencies"]),upload.single('image'), exportAgency);
ProfileRouter.post('/insurance',middlewareRoleManager(["insurance",'all']),upload.single('image'), insurance);


export default ProfileRouter;