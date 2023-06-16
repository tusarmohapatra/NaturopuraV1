const mysql = require("mysql2");
// Open the connection to MySQL server
const connection = mysql.createConnection({
    host: process.env.DATABASE_HOST_NAME,
    user: process.env.DATABASE_USER_NAME,
    password: process.env.DATABASE_USER_PASSWORD,
  });
  
  // Run create database statement
  connection.query(
    `CREATE DATABASE IF NOT EXISTS ${process.env.SERVICE_DATABASE_NAME}`,
    function (err, results) {
      console.log(results);
      console.log(err);
    }
  );
  
  // Close the connection
connection.end();
  
  
  