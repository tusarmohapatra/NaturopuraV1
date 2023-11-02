const multerS3 = require('multer-s3');
const multer = require('multer'); 
const path = require('path'); 

import { Request } from 'express';

import s3 from '../s3.util';

const upload = multer({
    storage: multerS3({
        s3,
        acl: 'public-read',
        bucket: 'naturopura-image-bucket',
        contentType: multerS3.AUTO_CONTENT_TYPE,
        key: (req:Request, file:any, cb:any) => {   
            const fileName = `${Date.now()}_${Math.round(Math.random() * 1E9)}`;
            cb(null, `${fileName}${path.extname(file.originalname)}`);
        },
    })
});

export default upload;