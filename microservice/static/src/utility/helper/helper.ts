///

const mysql = require("mysql2");

////
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

// export function generateAccessToken(date:{name:string}) {
//   return jwt.sign(username, env.TOKEN_SECRET, { expiresIn: '1800s' });
// }
