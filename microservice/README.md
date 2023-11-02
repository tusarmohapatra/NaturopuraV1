# nodejs-microservices
Sample Node.js project to show how to create node apps with microservices architecture.




## Prerequisites

- Docker: [Installation Guide](https://docs.docker.com/get-docker/)
- Docker Compose: [Installation Guide](https://docs.docker.com/compose/install/)

## Clone the repository

   ```bash
   git clone [<repository_url>](https://github.com/Naturopura/NaturopuraV1.git)

   ```


## Copy the environment


```
cp .env.example .env

cp auth/.env.example auth/.env

cp frontend/admin/.env.example frontend/admin/.env

cp frontend/user/.env.example frontend/user/.env
```


## Build the docker

```
docker-compose build
```


## Create node_module in every directory

```
npm install
```

## Run the docker 

```
docker-compose up -d
```


apt-get install apt-transport-https software-properties-common ca-certificates curl gnupg lsb-release -y



