
const express = require('express');

const HttpStatus = require('http-status-codes');

const service = require('../services/trusted-cas-service');

const { validateNewTrustedCA, validateUpdTrustedCA } = require('../core/schema-validator');

const router = express.Router();

router.route('/ca')
  /* Root CA Certificate */
  .get(async (req, res) => {
    const result = await service.getRootCertificate();
    res.status(HttpStatus.OK).json(result);
  });

router.route('/ca/crl')
  /* Latest CRL issued by the Root CA */
  .get(async (req, res) => {
    const result = await service.getRootCRL();
    res.status(HttpStatus.OK).json(result);
  });

router.route('/throw-away/ca')
  /* retrieves the certificate from the root CA without needing the JWT token.
   * Used only by services behind the API gateway */
  .get(async (req, res) => {
    const result = await service.getRootCertificate();
    res.status(HttpStatus.OK).json(result);
  });

router.route('/throw-away/ca/crl')
  /* Latest CRL issued by the Root CA without needing the JWT token.
   * Used only by services behind the API gateway */
  .get(async (req, res) => {
    const result = await service.getRootCRL(req.query.update === 'true');
    res.status(HttpStatus.OK).json(result);
  });

router.route('/trusted-cas')
  /* Register Trusted CA Certificate */
  .post(validateNewTrustedCA(), (req, res) => {
    res.sendStatus(201);
  })
  /* List Trusted CA Certificates */
  .get((req, res) => {
    res.sendStatus(200);
  });

router.route('/trusted-cas/:caCertificateFingerprint')
  /* Delete Trusted CA Certificate */
  .delete((req, res) => {
    res.sendStatus(204);
  })
  /* Update Trusted CA Certificate */
  .patch(validateUpdTrustedCA(), (req, res) => {
    res.sendStatus(204);
  })
  /* Get Trusted CA Certificate */
  .get((req, res) => {
    res.sendStatus(200);
  });

module.exports = router;
