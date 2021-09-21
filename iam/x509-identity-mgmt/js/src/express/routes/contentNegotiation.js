const HttpStatus = require('http-status-codes');

const ContentType = {
  pem: 'application/x-pem-file',
  json: 'application/json',
};

/**
 * Middleware builder function used to check if the specified content types are
 * acceptable, based on the request’s Accept HTTP header field.
 *
 * @param {array}  accepts      array with all supported MIME types. The order of the
 *                              array is significant (server preferred order)
 * @param {object} contentTypes object whose attributes are used for serving different
 *                              representations of a resource at the same URI.
 * @returns Returns a Middleware.
 */
function contentNegotiation(accepts, contentTypes) {
  return (req, res) => {
    // The response object must have been previously formed with the necessary
    // data to build the content in all supported representations!

    // The method returns the best match, or if none of the specified content
    // types is acceptable, returns false (in which case, the application
    // should respond with 406 "Not Acceptable").
    const accept = req.accepts(accepts);

    if (accept === false || !Reflect.has(contentTypes, accept)) {
      res.sendStatus(HttpStatus.NOT_ACCEPTABLE);
    } else {
      // By default express framework encodes all responses in UTF-8
      res.set('Content-Type', `${accept}; charset=utf-8`);

      // serves the content in the representation accepted by the client.
      Reflect.get(contentTypes, accept)(res);
    }
  };
}

module.exports = {
  ContentType,
  contentNegotiation,
};
