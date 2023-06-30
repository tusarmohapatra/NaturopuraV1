import axios from "axios";

export const loginUser = async (credentials: {
  signature: string;
  key: string;
}) => {
  try {
    const response = await axios.post(
      "http://localhost:8080/auth/admin/login",
      credentials
    );
    return response.data;
  } catch (error: any) {
    throw error;
  }
};
