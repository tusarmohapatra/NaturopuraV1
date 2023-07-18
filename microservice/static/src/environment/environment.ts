import { cleanEnv, str, email, json,num } from "envalid";

const env = cleanEnv(process.env, {
  NODE_ENV: str({ default: "dev" }),
  PORT: num({ default: 3000 }),
  MONGODB_URL: str({ default: "mongodb://mongo:root@mongodb:27017/" }),
  PRODUCT_SERVICE_DATABASE_NAME:str(),
  PRODUCT_DATABASE_USER_NAME:str(),
  PRODUCT_DATABASE_USER_PASSWORD:str(),
  DATABASE_HOST_NAME:str(),
  TOKEN_SECRET: str()
});


export default env;