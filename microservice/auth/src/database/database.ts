import {Sequelize} from 'sequelize';

import {colorLog, createDbForService} from '../utility/helper/helper';

import env from '../environment/environment';

const sequelize = new Sequelize(
  env.SERVICE_DATABASE_NAME,
  env.DATABASE_USER_NAME,
  env.DATABASE_USER_PASSWORD,
  {
    host: env.DATABASE_HOST_NAME, // Replace with the host of your Docker MySQL container
    dialect: 'mysql',
  }
);

createDbForService()
  .then((result: any) => {
    connectToDb();
  })
  .catch((err: any) => {
    console.log(err);
  });

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
