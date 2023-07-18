import Product from "./product.model";

const { Sequelize, Model, DataTypes } = require('sequelize');

const sequelize = require('../database/database')

const Review = sequelize.define('review', {
  id: {
    primaryKey: true,
    autoIncrement: true,
    type: DataTypes.INTEGER.UNSIGNED,
  },
  productId: {
    type: DataTypes.INTEGER.UNSIGNED,
    allowNull: false,
  },
  ratingValue: {
    type: DataTypes.ENUM('1','2','3','4','5'),
    defaultValue:'5',
    allowNull: false,
  },
  feedback: {
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
  updatedBy:{
    type: DataTypes.INTEGER,
    allowNull:true,
   }
});
Review.belongsTo(Product, {foreignKey: 'productId' });
export default  Review;
