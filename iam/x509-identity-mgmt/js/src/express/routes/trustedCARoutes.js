const HttpStatus = require("http-status-codes");

const sanitizeParams = require("./sanitizeParams");

const CA_MODEL = "trustedCAModel";
const CA_SERVICE = "trustedCAService";

module.exports = ({ mountPoint, schemaValidator, errorTemplate }) => {
  const { validateNewTrustedCA, validateUpdTrustedCA } = schemaValidator;
  const { BadRequest } = errorTemplate;

  const trustedCAsRoute = {
    mountPoint,
    name: "trusted-cas-route",
    path: ["/trusted-cas"],
    handlers: [
      {
        /* Register Trusted CA Certificate */
        method: "post",
        middleware: [
          validateNewTrustedCA(),
          async (req, res) => {
            const pemArr = req.body.caPem.match(sanitizeParams.certRegExp);
            if (!pemArr || pemArr.length > 1) {
              throw BadRequest(
                "Only one CA certificate is expected per request.",
              );
            }

            const [caPem] = pemArr.map(
              (el) => sanitizeParams.sanitizeLineBreaks(el)
            );

            const allowAutoRegistration = req.body.allowAutoRegistration || false;

            const caService = req.scope.resolve(CA_SERVICE);

            const result = await caService.registerCertificate({
              caPem,
              allowAutoRegistration,
            });

            res.status(HttpStatus.CREATED).json(result);
          },
        ],
      },
      {
        /* List Trusted CA Certificates */
        method: "get",
        middleware: [
          async (req, res) => {
            const caModel = req.scope.resolve(CA_MODEL);
            const caService = req.scope.resolve(CA_SERVICE);

            const queryFields = caModel.parseProjectionFields(req.query.fields);
            const filterFields = caModel.parseConditionFields(req.query);
            const sortBy = caModel.parseSortBy(req.query.sortBy);

            const { itemCount, results } = await caService.listCertificates(
              queryFields, filterFields, req.query.limit, req.offset, sortBy,
            );

            results.forEach((cert) => caModel.sanitizeFields(cert));

            const paging = req.getPaging(itemCount);
            res.status(HttpStatus.OK).json({ paging, "trusted-cas": results });
          },
        ],
      },
    ],
  };

  const trustedCAsCAFingerprintRoute = {
    mountPoint,
    name: "trusted-cas-fingerprint-route",
    path: ["/trusted-cas/:caFingerprint"],
    params: [
      {
        name: "caFingerprint",
        trigger: sanitizeParams.fingerprintHandler,
      },
    ],
    handlers: [
      {
        /* Get Trusted CA Certificate */
        method: "get",
        middleware: [
          async (req, res) => {
            const caModel = req.scope.resolve(CA_MODEL);
            const caService = req.scope.resolve(CA_SERVICE);

            const { caFingerprint } = req.params;
            const queryFields = caModel.parseProjectionFields(req.query.fields);
            const filterFields = caModel.parseConditionFields({
              caFingerprint,
            });

            const result = await caService.getCertificate(
              queryFields,
              filterFields,
            );
            caModel.sanitizeFields(result);

            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
      {
        /* Update Trusted CA Certificate */
        method: "patch",
        middleware: [
          validateUpdTrustedCA(),
          async (req, res) => {
            const caModel = req.scope.resolve(CA_MODEL);
            const caService = req.scope.resolve(CA_SERVICE);

            const { caFingerprint } = req.params;
            const filterFields = caModel.parseConditionFields({
              caFingerprint,
            });

            await caService.changeAutoRegistration(
              filterFields,
              req.body.allowAutoRegistration,
            );

            res.sendStatus(HttpStatus.NO_CONTENT);
          },
        ],
      },
      {
        /* Delete Trusted CA Certificate */
        method: "delete",
        middleware: [
          async (req, res) => {
            const caModel = req.scope.resolve(CA_MODEL);
            const caService = req.scope.resolve(CA_SERVICE);

            const { caFingerprint } = req.params;
            const queryFields = caModel.parseProjectionFields(null);
            const filterFields = caModel.parseConditionFields({
              caFingerprint,
            });

            const certToRemove = await caService.getCertificate(
              queryFields,
              filterFields,
            );
            await caService.deleteCertificate(certToRemove);

            res.sendStatus(HttpStatus.NO_CONTENT);
          },
        ],
      },
    ],
  };

  return [trustedCAsRoute, trustedCAsCAFingerprintRoute];
};
