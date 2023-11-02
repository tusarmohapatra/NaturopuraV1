import { Router, Request, Response } from "express";
import {
  createChemicalCategory,
  deleteChemicalCatCategory,
  updateChemicalCategory,
} from "../controller/chemical.controller";
import { createCategory, deleteCategory, updateCategory } from "../controller/product.controller";
import { createEquipmentCategory, deleteEquipmentCategory, updateEquipmentCategory } from "../controller/equipment.controller";

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
