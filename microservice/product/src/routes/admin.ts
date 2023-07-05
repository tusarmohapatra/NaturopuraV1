import { Router, Request, Response } from "express";
import {
  createCategory,
  createChemicalCategory,
  createEquipmentCategory,
  deleteCategory,
  deleteChemicalCatCategory,
  deleteEquipmentCategory,
  updateCategory,
  updateChemicalCategory,
  updateEquipmentCategory,
} from "../controller/category.controller";

const router: Router = Router();

// Define a route for user registration
router.post("/create-category", createCategory);
router.patch("/update-category", updateCategory);
router.delete("/delete-category/:id", deleteCategory);
//equipment
router.post("/create-category-equ", createEquipmentCategory);
router.patch("/update-category-equ", updateEquipmentCategory);
router.delete("/delete-category-equ/:id", deleteEquipmentCategory);
//chemical category
router.post("/create-category-chemical", createChemicalCategory);
router.patch("/update-category-chemical", updateChemicalCategory);
router.delete("/delete-category-chemical/:id", deleteChemicalCatCategory);
// deleteEquipmentCategory
export default router;
