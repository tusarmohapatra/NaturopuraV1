const { Sequelize } = require('sequelize');

const sequelize = new Sequelize('app_db', 'root', 'root', {
  host: 'mysql', // Replace with the host of your Docker MySQL container
  dialect: 'mysql',
});
sequelize
  .authenticate()
  .then(() => console.log('Connection has been established successfully.'))
  .catch(error => console.error('Unable to connect to the database:', error));

module.exports = sequelize;
