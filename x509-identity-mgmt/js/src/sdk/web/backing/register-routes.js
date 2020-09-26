const express = require('express');

const allowedRoutingMethods = (method) => [
  'all', 'ws', 'checkout', 'copy', 'delete', 'get', 'head',
  'lock', 'merge', 'mkactivity', 'mkcol', 'move', 'm-search',
  'notify', 'options', 'patch', 'post', 'purge', 'put',
  'report', 'search', 'subscribe', 'trace', 'unlock', 'unsubscribe',
].includes(method);

const sanitize = (obj, logger) => {
  const routePattern = {
    name: '',
    mountPoint: '',
    path: [],
    params: [],
    handlers: [],
  };
  logger.debug(`Sanitizing the object so that only the pattern remains: ${routePattern}`);

  const subset = Object.fromEntries(
    Object.entries(obj).filter(([key]) => Object.keys(routePattern).includes(key)),
  );

  if (subset.name && typeof subset.name !== 'string') {
    delete subset.name;
  }

  if (subset.mountPoint && typeof subset.mountPoint !== 'string') {
    delete subset.mountPoint;
  }

  if (subset.path && !Array.isArray(subset.path)) {
    subset.path = [subset.path];
  }

  if (subset.params && !Array.isArray(subset.params)) {
    subset.params = [subset.params];
  }

  if (subset.handlers && !Array.isArray(subset.handlers)) {
    subset.handlers = [subset.handlers];
  }

  return { ...routePattern, ...subset };
};

const checkPath = ({ path }, logger) => {
  logger.debug('Checking if the object has valid route path values...');
  if (!path.length) {
    logger.debug('The object has no route path value. This is a problem!');
    return false;
  }

  const checked = path.every((value) => {
    if (!value) {
      logger.debug('Route path value has not been defined, but it should be!');
      return false;
    }
    if (typeof value !== 'string' && !(value instanceof RegExp)) {
      logger.debug('Route path value must be a string or instance of RegExp!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('Route path values are checked!');
    return true;
  }
  logger.debug('Route path values are not implemented correctly. This is a problem!');
  return false;
};

const checkParams = ({ params }, logger) => {
  logger.debug('Checking if the object has triggers for parameters declared in the route path...');
  if (!params || !params.length) {
    logger.debug('The object has no triggers for parameters declared in the route path. OK no problem!');
    return true;
  }

  const checked = params.every(({ name, trigger }) => {
    logger.debug(`Checking the trigger for the parameter: '${name}'`);
    if (!name) {
      logger.debug('Parameter Name has not been defined, but it should be!');
      return false;
    }
    if (!trigger) {
      logger.debug('Parameter Trigger has not been defined, but it should be!');
      return false;
    }
    if (typeof name !== 'string') {
      logger.debug('Parameter Name must be a string!');
      return false;
    }
    if (typeof trigger !== 'function') {
      logger.debug('Parameter Trigger must be a function!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('Triggers for the parameters on the route path are checked!');
    return true;
  }
  logger.debug('Triggers for the parameters in the route path are not implemented correctly. This is a problem!');
  return false;
};

const checkHandlers = ({ handlers }, logger) => {
  logger.debug('Checking if the object has handlers to handle requests in the route path...');
  if (!handlers.length) {
    logger.debug('The object has no handlers to handle requests in the route path. This is a problem!');
    return false;
  }

  const checked = handlers.every(({ method, middleware }) => {
    if (!method) {
      logger.debug("Handler's Routing Method has not been defined, but it should be!");
      return false;
    }
    if (!middleware) {
      logger.debug('Handler Middleware has not been defined, but it should be!');
      return false;
    }
    if (!allowedRoutingMethods.includes(method)) {
      logger.debug('The handler does not have a valid Routing Method!');
      return false;
    }
    if ((typeof middleware !== 'function' && !Array.isArray(middleware))
        || (Array.isArray(middleware) && middleware.some((mid) => typeof mid !== 'function'))) {
      logger.debug('The middleware of the handler must be a function or an array of functions!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('Request handlers on the route path are checked!');
    return true;
  }
  logger.debug('Request handlers on the route path are not implemented correctly. This is a problem!');
  return false;
};

module.exports = (routesToBeRegistered, framework, logger) => {
  let routes = routesToBeRegistered;
  if (!Array.isArray(routes)) {
    routes = [routes];
  }

  const mountingMap = new Map();

  routes.forEach((routeToBeRegistered) => {
    const route = sanitize(routeToBeRegistered, logger);
    if (checkPath(route, logger) && checkParams(route, logger) && checkHandlers(route, logger)) {
      let { mountPoint } = route;
      if (!mountPoint.startsWith('/')) {
        mountPoint = `/${mountPoint}`;
      }

      let mountRouter = mountingMap.get(mountPoint);
      if (!mountRouter) {
        mountRouter = express.Router();
        mountingMap.set(mountPoint, mountRouter);
      }

      const expressRoute = mountRouter.route(route.path);
      route.params.forEach(({ name, trigger }) => mountRouter.param(name, trigger));
      route.handlers.forEach(({ method, middleware }) => expressRoute[method](middleware));

      logger.debug(`Route registered: ${route.name}`);
      logger.debug(`---> Mount point: ${mountPoint}`);
      logger.debug(`---> Path pattern: ${route.path.reduce((acc, cur) => `${acc}, ${cur.toString()}`)}`);
    } else {
      logger.debug(`Route not registered: ${route.name}`);
    }
  });

  mountingMap.forEach((mountRouter, mountPoint) => {
    framework.use(mountPoint, mountRouter);
  });
};
