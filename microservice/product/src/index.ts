import express from "express";
import { consumeEvent } from "./event/event";
import Product from "./model/product.model";
import { colorLog } from "./utility/helper/helper";
require("express-async-errors");
const jwt = require("jsonwebtoken");
const sequelize = require("./database/database");
require("dotenv").config();
const app = express();
app.use(express.json());
const port = process.env.PORT || 3000;
// Creating all the tables defined in user
sequelize
  .sync()
  .then((result: any) => {
    console.log("result");
  })
  .catch((err: any) => {
    console.log("error");
  });

async function syncDatabase() {
  try {
    await Product.sync({ force: true });
  } catch (error) {
    console.log(colorLog('Waiting for user registered events...','FgRed'));
  }
}
syncDatabase();


async function establishConnection() {
  await consumeEvent()
    .then((result) => {
      console.log(colorLog(result,'BgGreen'));
    })
    .catch((error) => {
      console.error(error);
      setTimeout(() => {
        establishConnection();
      }, 200);
    });
}

app.listen(port, async () => {
  establishConnection();
  console.log(colorLog(`Order Service at ${port}`,'FgGreen'));
});
