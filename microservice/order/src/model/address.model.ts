const { Sequelize, Model, DataTypes } = require('sequelize');

const sequelize = require('../database/database')

const Address = sequelize.define('address', {
  id: {
    primaryKey: true,
    autoIncrement: true,
    type: DataTypes.INTEGER.UNSIGNED,
  },
  userId: {
    type: DataTypes.INTEGER.UNSIGNED,
    allowNull: false,
  },
  orderId: {
    type: DataTypes.INTEGER.UNSIGNED,
    allowNull: true,
  },
  address_line1: {
    type: DataTypes.STRING,
    allowNull: false
  },
  address_line2: {
    type: DataTypes.STRING,
    allowNull: true
  },
  city: {
    type: DataTypes.STRING,
    allowNull: true
  },
  state: {
    type: DataTypes.STRING,
    allowNull: true
  },
  postal_code: {
    type: DataTypes.INTEGER,
    allowNull: true
  },
  country: {
    type: DataTypes.STRING,
    allowNull: true
  },
  address_type: {
    type: DataTypes.ENUM,
    values: ['billing', 'shipping','user' ],
    allowNull: false,
    defaultValue: 'user'
  },
  updatedBy:{
    type: DataTypes.INTEGER,
    allowNull:true,
   },
  deletedAt: {
    type: DataTypes.DATE,
    allowNull: true
  },
});

export default  Address;
