import { cleanEnv, str, email, json,num } from "envalid";

const env = cleanEnv(process.env, {
  NODE_ENV: str({ default: "dev" }),
  PORT: num({ default: 3000 }),
  DATABASE_HOST_NAME: str({ default: "mysql" }),
  DATABASE_USER_NAME: str({ default: "root" }),
  DATABASE_USER_PASSWORD: str({ default: "root" }),
  SERVICE_DATABASE_NAME: str({ default: "usersDb" }),
  TOKEN_SECRET: str(),
  AWS_S3_REGION: str(),
  AWS_S3_ACCESS_KEY_ID: str(),
  AWS_S3_SECRET_ACCESS_KEY: str(),
  AWS_S3_BUCKET_NAME:str()
});


export default env;