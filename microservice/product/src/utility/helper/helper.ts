///
import { response, Request, Response, NextFunction } from "express";
import env from "../../environment/environment";
import { SessionData } from "express-session";
const mysql = require("mysql2");
const jwt = require("jsonwebtoken");
import "express-session";


export async function createDbForService() {
  const connection = mysql.createConnection({
    host: env.DATABASE_HOST_NAME,
    user: env.DATABASE_USER_NAME,
    password: env.DATABASE_USER_PASSWORD,
  });

  return new Promise((resolve, reject) => {
    connection.query(
      `CREATE DATABASE IF NOT EXISTS ${env.SERVICE_DATABASE_NAME}`,
      function (err: any, results: any) {
        if (err) {
          throw err;
        } else {
          return resolve(results);
        }
      }
    );

    connection.end();
  });
}

export const middlewareRoleManager = (accessFor: string) => {
  return (req: Request, res: Response, next: NextFunction) => {
    if (req.headers?.authorization) {
      const token = req.headers?.authorization.replace("Bearer ", "");
      const decoded = jwt.verify(token, env.TOKEN_SECRET);
      if (decoded?.role === accessFor) {
        next();
      } else {
        return res
          .status(401)
          .json(
            createErrorResponse(
              "UNABLE_TO_AUTHORIZE",
              "you are not authorize for this route"
            )
          );
      }
    } else {
      return res
        .status(401)
        .json(
          createErrorResponse(
            "UNABLE_TO_AUTHORIZE",
            "you are not authorize for this route"
          )
        );
    }

    // Call next() to pass control to the next middleware or route handler
  };
};

interface Person {
  isActive: boolean ;
  id: number;
  firstName:string;
  lastName:string;
  role:string;
  email:string;
  iat:number;
  exp:number;
}

export const getPayloadFromToken = (req: Request): Person  => {
  if (req.headers?.authorization) {
    const token = req.headers?.authorization.replace("Bearer ", "");
    const decoded = jwt.verify(token, env.TOKEN_SECRET);
    return decoded;
  } else {
    return {
      isActive: true,
      id: 0,
      firstName: "",
      lastName: "",
      role: "",
      email: "",
      iat: 0,
      exp: 0,
    };
  }
};


export function colorLog(val: any, color: string) {
  const Reset = "\x1b[0m";
  const Bright = "\x1b[1m";
  const Dim = "\x1b[2m";
  const Underscore = "\x1b[4m";
  const Blink = "\x1b[5m";
  const Reverse = "\x1b[7m";
  const Hidden = "\x1b[8m";
  const FgBlack = "\x1b[30m";
  const FgRed = "\x1b[31m";
  const FgGreen = "\x1b[32m";
  const FgYellow = "\x1b[33m";
  const FgBlue = "\x1b[34m";
  const FgMagenta = "\x1b[35m";
  const FgCyan = "\x1b[36m";
  const FgWhite = "\x1b[37m";
  const FgGray = "\x1b[90m";
  const BgBlack = "\x1b[40m";

  const BgRed = "\x1b[41m";
  const BgGreen = "\x1b[42m";
  const BgYellow = "\x1b[43m";
  const BgBlue = "\x1b[44m";
  const BgMagenta = "\x1b[45m";
  const BgCyan = "\x1b[46m";
  const BgWhite = "\x1b[47m";

  const BgGray = "\x1b[100m";

  let response = "";
  switch (color) {
    case "Dim":
      response = `${Dim}${val}${Reset}`;
      break;
    case "Underscore":
      response = `${Underscore}${val}${Reset}`;
      break;
    case "Blink":
      response = `${Blink}${val}${Reset}`;

      break;
    case "Reverse":
      response = `${Reverse}${val}${Reset}`;
      break;
    case "Hidden":
      response = `${Hidden}${val}${Reset}`;
      break;
    case "FgBlack":
      response = `${FgBlack}${val}${Reset}`;
      break;
    case "FgRed":
      response = `${FgRed}${val}${Reset}`;
      break;
    case "FgGreen":
      response = `${FgGreen}${val}${Reset}`;
      break;
    case "FgYellow":
      response = `${FgYellow}${val}${Reset}`;
      break;
    case "FgBlue":
      response = `${FgBlue}${val}${Reset}`;
      break;
    case "FgMagenta":
      response = `${FgMagenta}${val}${Reset}`;
      break;
    case "FgCyan":
      response = `${FgCyan}${val}${Reset}`;
      break;
    case "FgWhite":
      response = `${FgWhite}${val}${Reset}`;
      break;
    case "FgGray":
      response = `${FgGray}${val}${Reset}`;
      break;
    case "BgBlack":
      response = `${BgBlack}${val}${Reset}`;
      break;
    case "BgGray":
      response = `${BgGray}${val}${Reset}`;
      break;
    case "BgRed":
      response = `${BgRed}${val}${Reset}`;
      break;
    case "BgGreen":
      response = `${BgGreen}${val}${Reset}`;
      break;
    case "BgYellow":
      response = `${BgYellow}${val}${Reset}`;
      break;
    case "BgBlue":
      response = `${BgBlue}${val}${Reset}`;
      break;
    case "BgMagenta":
      response = `${BgMagenta}${val}${Reset}`;
      break;
    case "BgCyan":
      response = `${BgCyan}${val}${Reset}`;
      break;
    case "BgWhite":
      response = `${BgWhite}${val}${Reset}`;
      break;
    default:
      response = `${Bright}${val}${Reset}`;
      break;
  }

  return response;
}

export function createErrorResponse(
  code: string,
  message: string,
  details = {}
) {
  return {
    error: {
      code,
      message,
      details,
    },
  };
}

export function createSuccessResponse(
  messages: string,
  token: string = "",
  data = {}
) {
  return {
    success: true,
    data,
    messages,
    token,
  };
}