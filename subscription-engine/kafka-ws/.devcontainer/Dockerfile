# Modified version of the original devcontainer for development use of Kafka WS service

FROM node:10.16.3-alpine

# Configure apk and install packages
RUN apk update && apk add curl zsh git 2>&1

# Installing Oh My Zsh
# If you don't want to use zsh, you can change the shell in devcontainer.json
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Install eslint globally
RUN npm install -g eslint

RUN echo "NodeJS version: $(node --version)" \
    echo "NPM version: $(npm --version)"

