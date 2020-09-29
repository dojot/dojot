const { pagingMiddleware } = require('../sdk/web/backing/paginate');

module.exports = ({ limit, maxLimit }) => ({
  name: 'paginate-controller',
  middleware: pagingMiddleware(limit, maxLimit),
});
