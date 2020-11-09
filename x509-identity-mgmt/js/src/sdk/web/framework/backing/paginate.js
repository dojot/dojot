const paginate = require('express-paginate');

function addBehaviors(req, res) {
  req.getPaging = (itemCount) => {
    const pageCount = Math.ceil(itemCount / req.query.limit);

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

    const nextPage = (!res.locals.paginate.hasNextPages(pageCount)) ? null
      : {
        number: req.query.page + 1,
        url: res.locals.paginate.href(false),
      };

    return {
      previous: previousPage,
      current: currentPage,
      next: nextPage,
      totalItems: itemCount,
      totalPages: pageCount,
      limitPerPage: req.query.limit,
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
