///
import env from '../../environment/environment';
const mysql = require('mysql2');

////
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


// export function generateAccessToken(date:{name:string}) {
//   return jwt.sign(username, env.TOKEN_SECRET, { expiresIn: '1800s' });
// }


export function createErrorResponse(code:string, message:string, details = {}) {
  return {
    error: {
      code,
      message,
      details,
    },
  };
}

export function createSuccessResponse(messages:string,token:string = '',data = {}) {
  return {
    success: true,
    data,
    messages,
    token
  };
}