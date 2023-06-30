import express from 'express';
import { consumeEvent } from './event/event';
import Address from './model/address.model';
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



async function syncDatabase() {
  try {
    await Address.sync({ force: true });
  } catch (error) {
    console.log("error while create sync with user table");
  }
};
syncDatabase();

const port = process.env.PORT || 3000;
app.listen(port, async () => {
  await consumeEvent();
  console.log(`Order Service at ${port}`);
});


