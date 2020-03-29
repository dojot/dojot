FROM python:3.6-alpine3.10

WORKDIR /usr/src/app

COPY . .

RUN apk add --no-cache \
    curl \
    bash \
    gcc \
    g++ \
    libffi-dev \
    make \
    openssl-dev \
    redis

RUN pip install --no-cache-dir -r requirements/prod.txt
RUN pip install --no-cache-dir -r requirements/dev.txt

# This command will do nothing and prevent the container from exiting
# from lack of commands
CMD tail -f /dev/null
