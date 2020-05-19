
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
    res.send('OK!');
  });

router.route('/trusted-cas/:caCertificateFingerprint')
  /* Delete Trusted CA Certificate */
  .delete((req, res) => {
    res.send('OK!');
  })
  /* Update Trusted CA Certificate */
  .patch(validateUpdTrustedCA(), (req, res) => {
    res.send('OK!');
  })
  /* Get Trusted CA Certificate */
  .get((req, res) => {
    res.send('OK!');
  });

module.exports = router;
