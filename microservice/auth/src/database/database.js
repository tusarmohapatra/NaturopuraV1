
const { Sequelize } = require('sequelize');

require('./createUserDb.js')

const sequelize = new Sequelize(process.env.SERVICE_DATABASE_NAME, process.env.DATABASE_USER_NAME, process.env.DATABASE_USER_PASSWORD, {
  host: process.env.DATABASE_HOST_NAME, // Replace with the host of your Docker MySQL container
  dialect: 'mysql',
});
sequelize
  .authenticate()
  .then(() => console.log('Connection has been established successfully.'))
  .catch(error => console.error('Unable to connect to the database:', error));

module.exports = sequelize;
