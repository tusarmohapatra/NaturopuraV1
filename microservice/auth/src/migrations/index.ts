import User from "../model/user.model";
import UserMeta from "../model/userMeta.model";
import { colorLog } from "../utility/helper/helper";

const { QueryTypes } = require("sequelize");
const sequelize = require("../database/database");

export default async function migration() {

  // try {
  //   await sequelize.query(
  //     `ALTER TABLE ${User.getTableName()} ADD COLUMN updatedBy INT(10) UNSIGNED DEFAULT NULL;`,
  //     {
  //       nest: true,
  //       type: QueryTypes.ALTER,
  //     }
  //   );
  //   console.log(
  //     colorLog(
  //       `success wile alter ${User.getTableName()} add updatedBy`,
  //       "FgGreen"
  //     )
  //   );
  // } catch (error) {
  //   console.log(colorLog(error, "FgRed"));
  //   console.log(
  //     colorLog(
  //       `error ${error} wile alter ${User.getTableName()} add updatedBy`,
  //       "FgRed"
  //     )
  //   );
  // }

  // try {
  // const object =   await sequelize.query(
  //     `ALTER TABLE ${UserMeta.getTableName()} ADD COLUMN updatedBy INT(10) UNSIGNED DEFAULT NULL;`,
  //     {
  //       nest: true,
  //       type: QueryTypes.ALTER,
  //     }
  //   );
  //   console.log(
  //     colorLog(
  //       `success wile alter ${UserMeta.getTableName()} add updatedBy`,
  //       "FgGreen"
  //     )
  //   );
  // } catch (error) {
  //   console.log(
  //     colorLog(
  //       `error ${error} wile alter ${UserMeta.getTableName()} add updatedBy`,
  //       "FgRed"
  //     )
  //   );
  // }
}
