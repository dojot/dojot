const { pagingMiddleware } = require('../backing/paginate');

function createInterceptor(limit, maxLimit, path = '/') {
  return {
    path,
    name: 'paginate-interceptor',
    middleware: pagingMiddleware(limit, maxLimit),
  };
}

module.exports = ({ limit, maxLimit, path }) => createInterceptor(limit, maxLimit, path);
