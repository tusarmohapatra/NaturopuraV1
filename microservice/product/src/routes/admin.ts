import { Router, Request, Response } from 'express';
import { createCategory } from '../controller/admin.controller';

const router: Router = Router();

// Define a route for user registration
router.post('/create-category', createCategory);

export default router;