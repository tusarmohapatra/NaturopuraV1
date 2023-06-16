import env from "../environment/environment";

const mysql = require("mysql2");
// Open the connection to MySQL server
const connection = mysql.createConnection({
  host: env.DATABASE_HOST_NAME,
  user: env.DATABASE_USER_NAME,
  password: env.DATABASE_USER_PASSWORD,
});

// Run create database statement
connection.query(
  `CREATE DATABASE IF NOT EXISTS ${env.SERVICE_DATABASE_NAME}`,
  function (err: any, results: any) {
    console.log(results);
    console.log(err);
  }
);

// Close the connection
connection.end();
