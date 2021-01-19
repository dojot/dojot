FROM node:12-alpine

WORKDIR /usr/src/app

COPY  example/server/ .

RUN apk --no-cache add \
    curl

EXPOSE 8888

CMD [ "node", "server.js" ]