const { Sequelize, Model, DataTypes } = require('sequelize');

// import sequelize from '../database/database';
// import  from '../database/database';

const sequelize = require('../database/database')

const User = sequelize.define('user', {
  id: {
    primaryKey: true,
    autoIncrement: true,
    type: DataTypes.INTEGER.UNSIGNED,
  },
  firstName: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  lastName: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  role: {
    type: DataTypes.ENUM,
    values: ['admin', 'consumer', 'farmer','distributors','consultant','agricultural_chemicals','equipment_manufacturers','marketing_agencies','insurance'],
    allowNull: false,
    defaultValue: 'consumer'
  },
  email: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  isActive: {
    type: DataTypes.BOOLEAN,
    allowNull: false,
    defaultValue: true
  },
  password: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  deletedAt: {
    type: DataTypes.DATE,
    allowNull: true
  },
});

export default  User;
