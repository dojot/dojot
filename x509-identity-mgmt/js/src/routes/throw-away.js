const express = require('express');

const HttpStatus = require('http-status-codes');

const { validateRegOrGenCert } = require('../core/schema-validator');

const certService = require('../services/certificates-service');

const { BadRequest } = require('../core/errors');

const caService = require('../services/trusted-cas-service');

const router = express.Router();

router.route('/throw-away')
/* issue and throw away a certificate given a CSR.
 * Used only by services behind the API gateway */
  .post(validateRegOrGenCert(), async (req, res) => {
    let result = null;
    if (req.body.csr) {
      result = await certService.throwAwayCertificate(req.body);
    } else {
      throw BadRequest('It is necessary to inform the CSR for the certificate to be issued');
    }
    res.status(HttpStatus.CREATED).json(result);
  });

router.route('/throw-away/ca')
  /* retrieves the certificate from the root CA without needing the JWT token.
   * Used only by services behind the API gateway */
  .get(async (req, res) => {
    const result = await caService.getRootCertificate();
    res.status(HttpStatus.OK).json(result);
  });

router.route('/throw-away/ca/crl')
  /* Latest CRL issued by the Root CA without needing the JWT token.
   * Used only by services behind the API gateway */
  .get(async (req, res) => {
    const result = await caService.getRootCRL(req.query.update === 'true');
    res.status(HttpStatus.OK).json(result);
  });

module.exports = router;
