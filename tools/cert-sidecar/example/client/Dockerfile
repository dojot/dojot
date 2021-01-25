FROM  node:12-alpine

WORKDIR /usr/src/app

COPY  example/client/ .

RUN apk --no-cache add \
    curl

CMD [ "node", "client.js" ]