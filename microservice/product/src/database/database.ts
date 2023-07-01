import {Sequelize} from 'sequelize';

import {createDbForService} from '../utility/helper/helper';

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
    .then(() => console.log(`Connection has been established to ${env.SERVICE_DATABASE_NAME} successfully.`))
    .catch((error: any) =>
      console.error('Unable to connect to the database:', error)
    );
}
module.exports = sequelize;
