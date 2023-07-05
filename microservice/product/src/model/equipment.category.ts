const { Sequelize, Model, DataTypes } = require('sequelize');

const sequelize = require('../database/database')

const equipmentCategory = sequelize.define('equipmentCategory', {
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
  recommendedUse: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  keyFeatures: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  power_source: {
    type: DataTypes.ENUM('Electronic','IC','Manual'),
    allowNull: true,
  },
  maintenance_requirements: {
    type: DataTypes.STRING,
    allowNull: false,
  },
  warranty: {
    type: DataTypes.STRING,
    allowNull: false,
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

equipmentCategory.belongsTo(equipmentCategory, {foreignKey: 'parentCategory' });
export default  equipmentCategory;
