const express = require('express');

const HttpStatus = require('http-status-codes');

const paginate = require('express-paginate');

const { validateRegOrGenCert, validateChangeOwnerCert } = require('../core/schema-validator');

const service = require('../services/x509-certificates-service');

const { certificate: parser } = require('../db');

const router = express.Router();

router.route('/x509-certificates')
  /* Generate x.509 Certificate from CSR
   * (or also)
   * Register x.509 Certificate Issued by an External CA */
  .post(validateRegOrGenCert(), async (req, res) => {
    let result = null;
    if (!req.body.belongsTo) {
      req.body.belongsTo = {};
    }
    if (req.body.csr) {
      result = await service.generateCertificate(req.body, req.tenant);
    } else if (req.body.certificatePem) {
      result = await service.registerCertificate(req.body, req.tenant);
    }
    res.status(HttpStatus.CREATED).json(result);
  })
  /* List x.509 Certificates */
  .get(async (req, res) => {
    const queryFields = parser.getProjectionFields(req.query.fields);
    const filterFields = parser.getConditionFields(req.query, req.tenant);

    const { itemCount, results } = await service.listCertificates(
      queryFields, filterFields, req.query.limit, req.offset,
    );
    const pageCount = Math.ceil(itemCount / req.query.limit);

    res.status(HttpStatus.OK).json({
      paging: {
        currentPage: req.query.page,
        totalPages: pageCount,
        itemLimitPerPage: req.query.limit,
        totalItems: itemCount,
        pages: paginate.getArrayPages(req)(null, pageCount, req.query.page),
      },
      certificates: results,
    });
  });

router.route('/x509-certificates/:certificateFingerprint')
  /* Delete x.509 certificate */
  .delete(async (req, res) => {
    const fingerprint = req.params.certificateFingerprint.toUpperCase();
    const queryFields = parser.getProjectionFields(null).filter((f) => f !== '-_id');
    const filterFields = parser.getConditionFields({ fingerprint }, req.tenant);
    const certRecord = await service.getCertificate(queryFields, filterFields);
    await service.deleteCertificate(certRecord);
    res.sendStatus(HttpStatus.NO_CONTENT);
  })
  /* Get x.509 Certificate */
  .get(async (req, res) => {
    const fingerprint = req.params.certificateFingerprint.toUpperCase();
    const queryFields = parser.getProjectionFields(req.query.fields);
    const filterFields = parser.getConditionFields({ fingerprint }, req.tenant);
    const result = await service.getCertificate(queryFields, filterFields);
    res.status(HttpStatus.OK).json(result);
  })
  /* Change the Ownership of a Specified x.509 Certificate */
  .patch(validateChangeOwnerCert(), async (req, res) => {
    const fingerprint = req.params.certificateFingerprint.toUpperCase();
    const filterFields = parser.getConditionFields({ fingerprint }, req.tenant);
    await service.changeOwnership(filterFields, req.body.belongsTo);
    res.sendStatus(HttpStatus.NO_CONTENT);
  });

module.exports = router;
