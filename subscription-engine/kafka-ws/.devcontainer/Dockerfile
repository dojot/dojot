# Modified version of the original devcontainer for development use of Kafka WS service

FROM dojot/kafka-ws:development

# Configure apk and install packages
RUN apk --no-cache add curl \
    zsh \
    git

RUN npm install --only=dev

# Installing Oh My Zsh
# If you don't want to use zsh, you can change the shell in devcontainer.json
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
