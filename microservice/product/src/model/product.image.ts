const { Sequelize, Model, DataTypes } = require('sequelize');

const sequelize = require('../database/database')

const Image = sequelize.define('image', {
  id: {
    primaryKey: true,
    autoIncrement: true,
    type: DataTypes.INTEGER.UNSIGNED,
  },
  referenceId: {
    type: DataTypes.INTEGER.UNSIGNED,
    allowNull: false,
  },
  imageUrl: {
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

export default  Image;
