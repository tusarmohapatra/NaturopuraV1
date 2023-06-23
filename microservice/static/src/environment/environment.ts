import { cleanEnv, str, email, json,num } from "envalid";

const env = cleanEnv(process.env, {
  NODE_ENV: str({ default: "dev" }),
  PORT: num({ default: 3000 }),
  MONGODB_URL: str({ default: "mongodb://mongo:root@mongodb:27017/" }),
  TOKEN_SECRET: str()
});


export default env;