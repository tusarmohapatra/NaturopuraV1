const { Sequelize, Model, DataTypes } = require('sequelize');
const sequelize = require('../database/database');
const User = require('./user.model');


const UserMeta = sequelize.define('userMeta', {
  id: {
    primaryKey: true,
    autoIncrement: true,
    type: DataTypes.INTEGER.UNSIGNED,
  },
  userId: {
    type: DataTypes.INTEGER.UNSIGNED,
    allowNull: false,
  },
  addressId: {
    type: DataTypes.INTEGER.UNSIGNED,
    allowNull: true,
  },
  farmLocation: {
    type: DataTypes.STRING,
    allowNull: true,
  },
  farmSize: {
    type: DataTypes.STRING,
    allowNull: true,
  },
  farmingExperience: {
    type: DataTypes.STRING,
    allowNull: true,
  },
  preferredCommunicationMethod: {
    type: DataTypes.JSON,
    allowNull: true,
  },
  dateRegistered: {
    type: DataTypes.DATE,
    allowNull: true,
  },
  companyName: {
    type: DataTypes.STRING,
    allowNull: true,
  },
  productsProduce: {
    type: DataTypes.JSON,
    allowNull: true,
  },
  processingMethods: {
    type: DataTypes.JSON,
    allowNull: true,
  },
  productionCapacity: {
    type: DataTypes.STRING,
    allowNull: true,
  },
  storageCapacity: {
    type: DataTypes.STRING,
    allowNull: true,
  },
  storageFacilities: {
    type: DataTypes.BOOLEAN,
    allowNull: true,
    defaultValue:true
  },
  temperatureZones: {
    type: DataTypes.STRING,
    allowNull: true
  },
  governmentId: {
    type: DataTypes.STRING,
    allowNull: true
  },
  securityMeasures: {
    type: DataTypes.STRING,
    allowNull: true
  },
  productCategories: {
    type: DataTypes.JSON,
    allowNull: true,
  },
  equipmentCategories: {
    type: DataTypes.JSON,
    allowNull: true,
  },
  deliveryOptions: {
    type: DataTypes.BOOLEAN,
    allowNull: true,
  },
  deletedAt: {
    type: DataTypes.DATE,
    allowNull: true
  }
});
UserMeta.belongsTo(User, {foreignKey: 'user_id' });
module.exports = UserMeta;
