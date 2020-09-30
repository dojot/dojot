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
  logger.debug('\tSanitizing the route object so that only the pattern remains: ', routePattern);

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
  logger.debug('\tChecking if the route object has valid path values...');
  if (!path.length) {
    logger.debug('\tThe route object has no path value. This is a problem!');
    return false;
  }

  const checked = path.every((value) => {
    if (!value) {
      logger.debug('\tRoute path value has not been defined, but it should be!');
      return false;
    }
    if (typeof value !== 'string' && !(value instanceof RegExp)) {
      logger.debug('\tRoute path value must be a string or instance of RegExp!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('\tRoute path values are checked!');
    return true;
  }
  logger.debug('\tRoute path values are not implemented correctly. This is a problem!');
  return false;
};

const checkParams = ({ params }, logger) => {
  logger.debug('\tChecking if the route object has triggers for parameters declared in the route path...');
  if (!params || !params.length) {
    logger.debug('\tThe route object has no triggers for parameters declared in the route path. OK no problem!');
    return true;
  }

  const checked = params.every(({ name, trigger }) => {
    logger.debug(`\tChecking the trigger for the parameter: '${name}'`);
    if (!name) {
      logger.debug('\tParameter Name has not been defined, but it should be!');
      return false;
    }
    if (!trigger) {
      logger.debug('\tParameter Trigger has not been defined, but it should be!');
      return false;
    }
    if (typeof name !== 'string') {
      logger.debug('\tParameter Name must be a string!');
      return false;
    }
    if (typeof trigger !== 'function') {
      logger.debug('\tParameter Trigger must be a function!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('\tTriggers for the parameters on the route path are checked!');
    return true;
  }
  logger.debug('\tTriggers for the parameters in the route path are not implemented correctly. This is a problem!');
  return false;
};

const checkHandlers = ({ handlers }, logger) => {
  logger.debug('\tChecking if the route object has handlers to handle requests in the route path...');
  if (!handlers.length) {
    logger.debug('\tThe route object has no handlers to handle requests in the route path. This is a problem!');
    return false;
  }

  const checked = handlers.every(({ method, middleware }) => {
    if (!method) {
      logger.debug("\tHandler's Routing Method has not been defined, but it should be!");
      return false;
    }
    if (!middleware) {
      logger.debug('\tHandler Middleware has not been defined, but it should be!');
      return false;
    }
    if (!allowedRoutingMethods(method)) {
      logger.debug('\tThe handler does not have a valid Routing Method!');
      return false;
    }
    if ((typeof middleware !== 'function' && !Array.isArray(middleware))
        || (Array.isArray(middleware) && middleware.some((mid) => typeof mid !== 'function'))) {
      logger.debug('\tThe middleware of the handler must be a function or an array of functions!');
      return false;
    }
    return true;
  });

  if (checked) {
    logger.debug('\tRequest handlers on the route path are checked!');
    return true;
  }
  logger.debug('\tRequest handlers on the route path are not implemented correctly. This is a problem!');
  return false;
};

module.exports = (routesToBeRegistered, framework, logger) => {
  let routes = routesToBeRegistered;
  if (!Array.isArray(routes)) {
    routes = [routes];
  }

  const mountingMap = new Map();

  routes.forEach((routeToBeRegistered) => {
    logger.debug(`\tRoute to be registered: ${routeToBeRegistered.name}`);

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
      route.handlers.forEach(({ method, middleware }) => (
        (method === 'ws')
          ? mountRouter[method](
            route.path[0],
            (Array.isArray(middleware) ? middleware[0] : middleware),
          )
          : expressRoute[method](middleware)
      ));

      logger.debug(`\tRoute registered! -> ${route.name}`);
      logger.debug(`\t---> Mount point: ${mountPoint}`);
      logger.debug(`\t---> Path pattern: ${route.path.reduce((acc, cur) => `${acc}, ${cur.toString()}`)}`);
    } else {
      logger.debug(`\tRoute NOT registered! -> ${route.name}`);
    }
  });

  mountingMap.forEach((mountRouter, mountPoint) => {
    framework.use(mountPoint, mountRouter);
  });
};
