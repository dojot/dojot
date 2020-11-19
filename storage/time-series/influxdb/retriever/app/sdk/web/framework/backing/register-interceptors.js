const sanitize = (obj, logger) => {
  const interceptorPattern = {
    name: '',
    path: [],
    middleware: [],
  };
  logger.debug('\tSanitizing the interceptor so that only the pattern remains: ', interceptorPattern);

  const subset = Object.fromEntries(
    Object.entries(obj).filter(([key]) => Object.keys(interceptorPattern).includes(key)),
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

  return { ...interceptorPattern, ...subset };
};

const checkPath = ({ path }, logger) => {
  logger.debug('\tChecking if the interceptor has valid path values...');
  if (!path.length) {
    logger.debug("\tThe interceptor has no path value. Using '/' (root path)!");
    return true;
  }

  const checked = path.every((value) => {
    if (typeof value !== 'string' && !(value instanceof RegExp)) {
      logger.debug('\tThe path value must be a string or instance of RegExp!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('\tThe path values are checked!');
    return true;
  }
  logger.debug('\tThe path values are not implemented correctly. This is a problem!');
  return false;
};

const checkMiddleware = ({ middleware }, logger) => {
  logger.debug('\tChecking if the interceptor has middlewares to handle requests...');
  if (!middleware.length) {
    logger.debug('\tThe interceptor has no middleware to handle requests. This is a problem!');
    return false;
  }

  const checked = middleware.every((fn) => {
    if (!fn) {
      logger.debug('\tMiddleware has not been defined, but it should be!');
      return false;
    }
    if (typeof fn !== 'function') {
      logger.debug('\tThe middleware must be a function. This is a problem!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('\tMiddlewares to handle requests are checked!');
    return true;
  }
  logger.debug('\tMiddlewares to handle requests are not implemented correctly. This is a problem!');
  return false;
};

const registerInterceptors = (interceptorsToBeRegistered, framework, logger) => {
  let interceptors = interceptorsToBeRegistered;
  if (!Array.isArray(interceptors)) {
    interceptors = [interceptors];
  }

  interceptors.forEach((interceptorToBeRegistered) => {
    logger.debug('\tInterceptor to be registered: ', { name: interceptorToBeRegistered.name });

    const interceptor = sanitize(interceptorToBeRegistered, logger);

    if (checkPath(interceptor, logger) && checkMiddleware(interceptor, logger)) {
      if (interceptor.path.length) {
        framework.use(interceptor.path, interceptor.middleware);
      } else {
        framework.use(interceptor.middleware);
      }
      logger.debug('\tInterceptor registered! -> ', interceptor);
    } else {
      logger.debug('\tInterceptor NOT registered! -> ', interceptor);
    }
  });
};

module.exports = registerInterceptors;
