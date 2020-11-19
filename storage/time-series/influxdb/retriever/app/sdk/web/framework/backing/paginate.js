const paginate = require('express-paginate');


function addBehaviors(req, res) {
  if (!req.query.dateTo && req.query.dateFrom) {
    req.query.dateTo = new Date().toISOString();
  }

  req.getPaging = (currentTotalPage) => {
    const previousPage = (!res.locals.paginate.hasPreviousPages) ? null
      : {
        number: req.query.page - 1,
        url: res.locals.paginate.href(true),
      };

    const currentPage = {
      number: req.query.page,
      url: res.locals.paginate.href()
        .replace(`page=${req.query.page + 1}`, `page=${req.query.page}`),
    };

    // To avoid counts
    const nextPage = (currentTotalPage < req.query.limit) ? null
      : {
        number: req.query.page + 1,
        url: res.locals.paginate.href(false),
      };

    return {
      previous: previousPage,
      current: currentPage,
      next: nextPage,
    };
  };
}

const pagingMiddleware = (limit, maxLimit) => ([
  paginate.middleware(limit, maxLimit),
  (req, res, next) => {
    if (req.query.limit < 1) req.query.limit = limit;
    addBehaviors(req, res);
    next();
  },
]);

module.exports = { pagingMiddleware };
