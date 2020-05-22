FROM node:10.16.3-alpine

WORKDIR /opt/ws-client

COPY Client.js .
COPY package.json .
COPY package-lock.json .

RUN npm install

CMD ["node", "Client.js"]
