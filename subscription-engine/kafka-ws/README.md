# Kafka WS
The **Kafka WS** service provides support for pure WebSocket connections to retrieve data directly from Dojot's Kafka.

Work in progress...

# Development

Before running the program, you need to compile the Nearley parser:
```shell
npm run parser:compile
```

Now you can run the program:
```shell
npm run kafka-ws
```

If you want to run with nodemon:
```shell
npm run dev
```

## Development in VS Code

If you use VS Code, you can install the [Remote Container](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) plugin and use the provided container to code in a standardized environment.

After installing the plugin, open the directory which contains this code with VS Code. It will ask you whether you want to use the container.

# **Issues and help**

If you found a problem or need help, leave an issue in the [Dojot repository](https://github.com/dojot/dojot) and we will help you!
