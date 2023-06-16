import { Sequelize } from "sequelize";

require("./createUserDb");

import env from "../environment/environment";

const sequelize = new Sequelize(
  env.SERVICE_DATABASE_NAME,
  env.DATABASE_USER_NAME,
  process.env.DATABASE_USER_PASSWORD,
  {
    host: env.DATABASE_HOST_NAME, // Replace with the host of your Docker MySQL container
    dialect: "mysql",
  }
);
sequelize
  .authenticate()
  .then(() => console.log("Connection has been established successfully."))
  .catch((error: any) =>
    console.error("Unable to connect to the database:", error)
  );

module.exports =  sequelize;
