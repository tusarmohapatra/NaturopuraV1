import express from 'express';
require('express-async-errors');
const jwt = require('jsonwebtoken');
const sequelize = require('./database/database');

require('dotenv').config();
const app = express();
app.use(express.json());



// Creating all the tables defined in user
sequelize.sync().then((result:any) => {
  console.log('result');
 
}).catch((err:any) => {
  console.log('error');
});


const port = process.env.PORT || 3000;
app.listen(port, () => {
  console.log(`Order Service at ${port}`);
});


