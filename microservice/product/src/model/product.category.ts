const { Sequelize, Model, DataTypes } = require('sequelize');

const sequelize = require('../database/database')

const productCategory = sequelize.define('productCategory', {
  id: {
    primaryKey: true,
    autoIncrement: true,
    type: DataTypes.INTEGER.UNSIGNED,
  },
  name: {
    type: DataTypes.STRING(30),
    allowNull: false,
  },
  description: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  parentCategory: {
    type: DataTypes.INTEGER.UNSIGNED,
    allowNull: true,
  },
  recommendedStorageConditions: {
    type: DataTypes.ENUM('coolAndDryPlace','refrigerated','roomTemperature','freezer','dryPantry','airtightContainer','keepAwayFromDirectSunlight','humidityControlled','storeInOriginalPackaging','avoidTemperatureFluctuations','doNotFreeze','storeInDarkPlace','keepUpright','handleWithCare'),
    defaultValue:'handleWithCare',
    allowNull: false,
  },
  shelfLifeIndDays: {
    type: DataTypes.INTEGER,
    allowNull: true
  },
  harvestingSeason: {
    type: DataTypes.ENUM('spring','summer','fall','winter','year-round','earlySpring','lateSpring','earlySummer','lateSummer','earlyFall','lateFall','earlyWinter','lateWinter'),
    allowNull: true,
  },
  createdBy: {
    type: DataTypes.INTEGER.UNSIGNED,
    allowNull: false
  },
  deletedAt: {
    type: DataTypes.DATE,
    allowNull: true
  },
});
productCategory.belongsTo(productCategory, {foreignKey: 'parentCategory' });
export default  productCategory;
