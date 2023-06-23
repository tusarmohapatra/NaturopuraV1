import { MongoClient, MongoClientOptions } from 'mongodb';
import env from '../environment/environment';

const mongoOptions: MongoClientOptions = {
  retryWrites: true,
  w: 'majority',
};

const url = env.MONGODB_URL; // Update with your MongoDB connection URL

const connectToDatabase = async () => {
  try {
    const client = await MongoClient.connect(url, mongoOptions);
    console.log('Connected to MongoDB');

    const db = client.db('static'); // Replace with your database name

    return db;
  } catch (error) {
    console.error('Error connecting to MongoDB:', error);
    throw error;
  }
};

export default connectToDatabase;
