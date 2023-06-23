import express from 'express';
require('express-async-errors');
const sequelize = require('./database/database')
const jwt = require('jsonwebtoken');
import User  from './model/user.model';
import UserMeta from './model/userMeta.model';

import apiRouter from './routes/api'

require('dotenv').config();
const app = express();
app.use(express.json());

app.use('/auth', apiRouter);



async function syncDatabase() {
  try {
    await User.sync({ force: true });
  } catch (error) {
    console.log("error while create sync with user table");
  }

  try {
    await UserMeta.sync({ force: true });
  } catch (error) {
    console.log("error while create sync with UserMeta table");
  }
};
syncDatabase();

sequelize.sync().then((result:any) => {
  console.log('result');
 
}).catch((err:any) => {
  console.log('error');
});


const port = process.env.PORT || 3000;
app.listen(port, () => {
  console.log(`Auth Service at ${port}`);
});


