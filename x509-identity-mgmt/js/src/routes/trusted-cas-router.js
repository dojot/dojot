
const express = require('express');

const { validateNewTrustedCA, validateUpdTrustedCA } = require('../core/schema-validator');

const router = express.Router();

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
