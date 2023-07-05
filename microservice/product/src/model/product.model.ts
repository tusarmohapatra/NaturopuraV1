const { Sequelize, Model, DataTypes } = require('sequelize');

const sequelize = require('../database/database')

const Product = sequelize.define('product', {
  id: {
    primaryKey: true,
    autoIncrement: true,
    type: DataTypes.INTEGER.UNSIGNED,
  },
  title: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  description: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  quantity: {
    type: DataTypes.INTEGER,
    allowNull: false,
  },
  unit: {
    type: DataTypes.ENUM('Kg', 'Liter','pound','Each','dozen','Pack'),
    defaultValue:'Kg',
    allowNull: false
  },
  price: {
    type: DataTypes.FLOAT,
    allowNull: false
  },
  location: {
    type: DataTypes.STRING,
    allowNull: true
  },
  harvestDate: {
    type: DataTypes.DATE,
    allowNull: true
  },
  expiryAfter: {
    type: DataTypes.INTEGER,
    allowNull: true
  },
  status: {
    type: DataTypes.ENUM('AVAILABLE','SOLD','REJECT','PENDING'),
    defaultValue:'AVAILABLE',
    allowNull: false
  },
  productType: {
    type: DataTypes.ENUM('GROCERY','EQUIPMENT','CHEMICAL'),
    defaultValue:'GROCERY',
    allowNull: false
  },
  productCategory: {
    type: DataTypes.INTEGER.UNSIGNED,
    allowNull: false
  },
  deletedAt: {
    type: DataTypes.DATE,
    allowNull: true
  },
});

export default  Product;
