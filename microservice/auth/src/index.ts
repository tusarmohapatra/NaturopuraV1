import express from 'express';
require('express-async-errors');
const sequelize = require('./database/database')
const bcrypt = require('bcryptjs');
const jwt = require('jsonwebtoken');
import User  from './model/user.model';
import UserMeta from './model/userMeta.model';

import apiRouter from './routes/api'

require('dotenv').config();
const app = express();
app.use(express.json());

app.use('/api', apiRouter);




// Creating all the tables defined in user
sequelize.sync().then((result:any) => {
  console.log('result');
 
}).catch((err:any) => {
  console.log('error');
});


const port = process.env.PORT || 3000;
app.listen(port, () => {
  console.log(`Auth Service at ${port}`);
});


