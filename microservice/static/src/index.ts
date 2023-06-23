import express from 'express';
import connectToDatabase from './database/database';
import router from './routes/api';
require('express-async-errors');
const jwt = require('jsonwebtoken');

require('dotenv').config();
const app = express();
app.use(express.json());

app.use('/data', router);

const port = process.env.PORT || 3000;
connectToDatabase()
  .then((db) => {
    // Start the Express server after successful database connection
    app.listen(port, () => {
      console.log(`Server is running on port ${port}`);
    });
  })
  .catch((error) => {
    console.error('Failed to connect to the database:', error);
  });




