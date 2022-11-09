const { Logger } = require('@dojot/microservice-sdk');

const TAG = 'http-agent:device-validator-interceptor';

const logger = new Logger('http-agent');

function createInterceptorMiddleware({ deviceManagerService }) {
  return async (req, res, next) => {
    logger.debug(`Entrando no middleware ${TAG}`);
    const { tenant, deviceId } = req.body;
    logger.info(`Verificando device ${deviceId} do tenant ${tenant}`);
    const deviceInformation = await deviceManagerService.getDevice(tenant, deviceId);
    logger.debug('Obteve informações para o device:');
    logger.debug({ deviceInformation });

    if (deviceInformation.disabled) {
      const error = `Device ${deviceId} está desabilitado. A mensagem será descartada.`;
      logger.warn(error);
      return next(error);
    }
    logger.debug(`Saindo do middleware ${TAG}`);
    return next();
  };
}

function validateDevice({ deviceManagerService }) {
  logger.info(`Criando interceptor ${TAG}`);
  return {
    name: TAG,
    middleware: createInterceptorMiddleware({ deviceManagerService }),
  };
}

module.exports = validateDevice;
