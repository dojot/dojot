const sanitize = (obj, logger) => {
  const controllerPattern = {
    name: '',
    path: [],
    middleware: [],
  };
  logger.debug(`Sanitizing the object so that only the pattern remains: ${controllerPattern}`);

  const subset = Object.fromEntries(
    Object.entries(obj).filter(([key]) => Object.keys(controllerPattern).includes(key)),
  );

  if (subset.name && typeof subset.name !== 'string') {
    delete subset.name;
  }

  if (subset.path && !Array.isArray(subset.path)) {
    subset.path = [subset.path];
  }

  if (subset.middleware && !Array.isArray(subset.middleware)) {
    subset.middleware = [subset.middleware];
  }

  return { ...controllerPattern, ...subset };
};

const checkPath = ({ path }, logger) => {
  logger.debug('Checking if the object has valid path values...');
  if (!path.length) {
    logger.debug("The object has no path value. Using '/' (root path)!");
    return true;
  }

  const checked = path.every((value) => {
    if (typeof value !== 'string' && !(value instanceof RegExp)) {
      logger.debug('The path value must be a string or instance of RegExp!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('The path values are checked!');
    return true;
  }
  logger.debug('The path values are not implemented correctly. This is a problem!');
  return false;
};

const checkMiddleware = ({ middleware }, logger) => {
  logger.debug('Checking if the object has middlewares to handle requests...');
  if (!middleware.length) {
    logger.debug('The object has no middleware to handle requests. This is a problem!');
    return false;
  }

  const checked = middleware.every((fn) => {
    if (!fn) {
      logger.debug('Middleware has not been defined, but it should be!');
      return false;
    }
    if (typeof middleware !== 'function') {
      logger.debug('The middleware must be a function. This is a problem!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('Middlewares to handle requests are checked!');
    return true;
  }
  logger.debug('Middlewares to handle requests are not implemented correctly. This is a problem!');
  return false;
};

module.exports = (controllersToBeRegistered, framework, logger) => {
  let controllers = controllersToBeRegistered;
  if (!Array.isArray(controllers)) {
    controllers = [controllers];
  }

  controllers.forEach((controllerToBeRegistered) => {
    const controller = sanitize(controllerToBeRegistered, logger);
    if (checkPath(controller, logger) && checkMiddleware(controller, logger)) {
      if (controller.path.length) {
        framework.use(controller.path, controller.middleware);
        logger.debug(`Controller registered: ${controller.name}`);
        logger.debug(`---> Path pattern: ${controller.path.reduce((acc, cur) => `${acc}, ${cur.toString()}`)}`);
      } else {
        framework.use(controller.middleware);
        logger.debug(`Controller registered: ${controller.name}`);
      }
    } else {
      logger.debug(`Controller not registered: ${controller.name}`);
    }
  });
};
