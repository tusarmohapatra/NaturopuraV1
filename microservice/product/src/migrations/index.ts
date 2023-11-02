import chemicalCategory from "../model/chemical.category";
import equipmentCategory from "../model/equipment.category";
import productCategory from "../model/product.category";
import Image from "../model/product.image";
import Product from "../model/product.model";
import Review from "../model/product.review";
import { colorLog } from "../utility/helper/helper";

const { QueryTypes } = require("sequelize");
const sequelize = require("../database/database");

export default async function migration() {
  // try {
  //   await sequelize.query(
  //     `ALTER TABLE ${equipmentCategory.getTableName()} ADD COLUMN updatedBy INT(10) UNSIGNED DEFAULT NULL;`,
  //     {
  //       nest: true,
  //       type: QueryTypes.ALTER,
  //     }
  //   );
  //   console.log(
  //     colorLog(
  //       `success wile alter ${productCategory.getTableName()} add updatedBy`,
  //       "FgGreen"
  //     )
  //   );
  // } catch (error) {
  //   console.log(colorLog(error, "FgRed"));
  //   console.log(
  //     colorLog(
  //       `error ${error} wile alter ${productCategory.getTableName()} add updatedBy`,
  //       "FgRed"
  //     )
  //   );
  // }

  // try {
  //   await sequelize.query(
  //     `ALTER TABLE ${productCategory.getTableName()} ADD COLUMN updatedBy INT(10) UNSIGNED DEFAULT NULL;`,
  //     {
  //       nest: true,
  //       type: QueryTypes.ALTER,
  //     }
  //   );
  //   console.log(
  //     colorLog(
  //       `success wile alter ${productCategory.getTableName()} add updatedBy`,
  //       "FgGreen"
  //     )
  //   );
  // } catch (error) {
  //   console.log(colorLog(error, "FgRed"));
  //   console.log(
  //     colorLog(
  //       `error ${error} wile alter ${productCategory.getTableName()} add updatedBy`,
  //       "FgRed"
  //     )
  //   );
  // }

  // try {
  //   await sequelize.query(
  //     `ALTER TABLE ${Image.getTableName()} ADD COLUMN updatedBy INT(10) UNSIGNED DEFAULT NULL;`,
  //     {
  //       nest: true,
  //       type: QueryTypes.ALTER,
  //     }
  //   );
  //   console.log(
  //     colorLog(
  //       `success wile alter ${Image.getTableName()} add updatedBy`,
  //       "FgGreen"
  //     )
  //   );
  // } catch (error) {
  //   console.log(colorLog(error, "FgRed"));
  //   console.log(
  //     colorLog(
  //       `error ${error} wile alter ${Image.getTableName()} add updatedBy`,
  //       "FgRed"
  //     )
  //   );
  // }

  // try {
  //   await sequelize.query(
  //     `ALTER TABLE ${Product.getTableName()} ADD COLUMN updatedBy INT(10) UNSIGNED DEFAULT NULL;`,
  //     {
  //       nest: true,
  //       type: QueryTypes.ALTER,
  //     }
  //   );
  //   console.log(
  //     colorLog(
  //       `success wile alter ${Product.getTableName()} add updatedBy`,
  //       "FgGreen"
  //     )
  //   );
  // } catch (error) {
  //   console.log(colorLog(error, "FgRed"));
  //   console.log(
  //     colorLog(
  //       `error ${error} wile alter ${Product.getTableName()} add updatedBy`,
  //       "FgRed"
  //     )
  //   );
  // }

  // try {
  //   await sequelize.query(
  //     `ALTER TABLE ${Review.getTableName()} ADD COLUMN updatedBy INT(10) UNSIGNED DEFAULT NULL;`,
  //     {
  //       nest: true,
  //       type: QueryTypes.ALTER,
  //     }
  //   );
  //   console.log(
  //     colorLog(
  //       `success wile alter ${Review.getTableName()} add updatedBy`,
  //       "FgGreen"
  //     )
  //   );
  // } catch (error) {
  //   console.log(colorLog(error, "FgRed"));
  //   console.log(
  //     colorLog(
  //       `error ${error} wile alter ${Review.getTableName()} add updatedBy`,
  //       "FgRed"
  //     )
  //   );
  // }

  // try {
  //   await sequelize.query(
  //     `ALTER TABLE ${chemicalCategory.getTableName()} ADD COLUMN updatedBy INT(10) UNSIGNED DEFAULT NULL;`,
  //     {
  //       nest: true,
  //       type: QueryTypes.ALTER,
  //     }
  //   );
  //   console.log(
  //     colorLog(
  //       `success wile alter ${chemicalCategory.getTableName()} add updatedBy`,
  //       "FgGreen"
  //     )
  //   );
  // } catch (error) {
  //   console.log(
  //     colorLog(
  //       `error ${error} wile alter ${chemicalCategory.getTableName()} add updatedBy `,
  //       "FgRed"
  //     )
  //   );
  // }
}
