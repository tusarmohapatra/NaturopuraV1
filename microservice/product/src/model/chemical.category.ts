const { Sequelize, Model, DataTypes } = require('sequelize');

const sequelize = require('../database/database')

const chemicalCategory = sequelize.define('chemicalCategory', {
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
  type: {
    type: DataTypes.ENUM('FERTILIZER','HERBICIDE','INSECTICIDE','FUNGICIDE','OTHER'),
    defaultValue:'OTHER',
    allowNull: false,
  },
  activeIngredient: {
    type: DataTypes.ENUM('YES','NO'),
    defaultValue:'NO',
    allowNull: false,
  },
  applicationMethod:{
    type: DataTypes.STRING,
    allowNull: true,
  },
  safetyPrecautions:{
    type: DataTypes.STRING,
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
  updatedBy:{
    type: DataTypes.INTEGER,
    allowNull:true,
   }
});

chemicalCategory.belongsTo(chemicalCategory, {foreignKey: 'parentCategory' });
export default  chemicalCategory;
