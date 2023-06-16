# Stage 1: dev stage
FROM node:14-alpine as dev




WORKDIR /app

# Copy package.json and package-lock.json
COPY package*.json ./

# Install development dependencies
RUN npm install --save-dev nodemon

# Copy application source code
COPY . .

# # Build the application
# RUN npm run build
# Install development dependencies
RUN npm install

CMD ["npm", "start"]

FROM node:14-alpine as production

WORKDIR /app

# Copy package.json and package-lock.json
COPY package*.json ./

# Install production dependencies
RUN npm install --only=production

# Copy built application from the "dev" stage
COPY --from=dev /app/dist ./dist

# Set environment variables if needed
# ENV NODE_ENV=production

# Expose the port on which the service will run
EXPOSE 3000

# Start the application
CMD ["npm", "start"]
