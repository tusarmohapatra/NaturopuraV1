import env from "../environment/environment";

const { S3Client } = require('@aws-sdk/client-s3');

const config = {
    region: env.AWS_S3_REGION,
    credentials: {
        accessKeyId: env.AWS_S3_ACCESS_KEY_ID,
        secretAccessKey: env.AWS_S3_SECRET_ACCESS_KEY
    }
}
const s3 = new S3Client(config);

export default s3;