import { Router, Request, Response } from "express";
import {
  createCategory,
  createEquipmentCategory,
  deleteCategory,
  deleteEquipmentCategory,
  updateCategory,
  updateEquipmentCategory,
} from "../controller/admin.controller";

const router: Router = Router();

// Define a route for user registration
router.post("/create-category", createCategory);
router.patch("/update-category", updateCategory);
router.delete("/delete-category/:id", deleteCategory);
// category
router.post("/create-category-equ", createEquipmentCategory);
router.patch("/update-category-equ", updateEquipmentCategory);
router.delete("/delete-category-equ/:id", deleteEquipmentCategory);

// deleteEquipmentCategory
export default router;
