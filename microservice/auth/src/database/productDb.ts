import {Sequelize} from 'sequelize';

import {colorLog} from '../utility/helper/helper';

import env from '../environment/environment';

const sequelize = new Sequelize(
  env.PRODUCT_SERVICE_DATABASE_NAME,
  env.PRODUCT_DATABASE_USER_NAME,
  env.PRODUCT_DATABASE_USER_PASSWORD,
  {
    host: env.DATABASE_HOST_NAME, // Replace with the host of your Docker MySQL container
    dialect: 'mysql',
  }
);


function connectToDb() {
  sequelize
    .authenticate()
    .then(() => console.log(colorLog("Connection has been established successfully.", "BgGreen")))
    .catch((error: any) => {
      console.log(colorLog("Unable to connect to the database:", "BgRed"));
      console.error(error);
    });
}
module.exports = sequelize;
