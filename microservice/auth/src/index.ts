import express from "express";
require("express-async-errors");
const sequelize = require("./database/database");
const jwt = require("jsonwebtoken");
import User from "./model/user.model";
import UserMeta from "./model/userMeta.model";
const amqp = require("amqplib");
import apiRouter from "./routes/api";
import { colorLog } from "./utility/helper/helper";

require("dotenv").config();
const app = express();
app.use(express.json());

app.use((req, res, next) => {
  res.setHeader("Access-Control-Allow-Origin", "http://localhost:3002"); // Adjust the allowed origin as needed
  res.setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");
  next();
});

app.use("/auth", apiRouter);

async function syncDatabase() {
  try {
    await User.sync({ force: true });
  } catch (error) {
    console.log(colorLog("error while create sync with user table", "BgRed"));
  }

  try {
    await UserMeta.sync({ force: true });
  } catch (error) {
    console.log(
      colorLog("error while create sync with UserMeta table", "BgRed")
    );
  }
}
syncDatabase();

sequelize
  .sync()
  .then((result: any) => {
    console.log("result");
  })
  .catch((err: any) => {
    console.log("error");
  });

const port = process.env.PORT || 3000;
app.listen(port, () => {
  console.log(colorLog(`Auth Service at ${port}`, "BgGreen"));
});
