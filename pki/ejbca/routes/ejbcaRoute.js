const { logger } = require('@dojot/dojot-module-logger');
const ejbcaUtils = require('../utils/ejbcaUtils');
const config = require('../src/config');

const TAG = { filename: 'ejbca_routes' };

/* EJBCA ROUTES */
const ejbcaRoute = (app, client, myCache) => {
  /* CA routes */

  app.get('/ca', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    client.getAvailableCAs((err, caList) => {
      if (err) {
        logger.error('Error getting CA data.', TAG);
        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
      logger.debug('CA data retrieved.', TAG);
      return res.status(200).json({ CAs: caList });
    });
  });

  app.get('/ca/:cacn', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    const { cacn } = req.params;
    const args = { arg0: cacn };

    client.getLastCAChain(args, (err, cert) => {
      if (err) {
        logger.error('Error retrieving certificates', TAG);
        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }

      const responseParse = {
        certificateData: ejbcaUtils.convertCerttoX509(cert.return[0].certificateData),
      };


      logger.debug('Certificates retrieved', TAG);
      return res.status(200).json({ certificate: responseParse.certificateData });
    });
  });

  app.get('/ca/:cacn/certificate/:certsn', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    const { cacn } = req.params;
    const { certsn } = req.params;
    const args = { arg0: cacn, arg1: certsn };

    client.getCertificate(args, (err, cert) => {
      if (err) {
        logger.error('Error retrieving ca certificate', TAG);

        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }

      if (!cert) {
        return res.status(404).json({
          code: 404,
          message: 'No certificate found',
          moreinfo: config.ejbcaConf.apiURL,
        });
      }

      logger.debug('CA certificate retrieved', TAG);
      return res.status(200).json({ certificate: cert });
    });
  });

  app.delete('/ca/:cacn/certificate/:certsn', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    const { cacn } = req.params;
    const { certsn } = req.params;

    let reasonCode = ejbcaUtils.reasons.UNSPECIFIED;

    if (req.query.reason) {
      if (req.query.reason in ejbcaUtils.reasons) {
        reasonCode = ejbcaUtils.reasons[req.query.reason];
      } else {
        logger.error('Inexistent reason', TAG);
        return res.status(404).json({
          code: 404,
          message: 'Inexistent reason code',
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
    }
    const args = { arg0: cacn, arg1: certsn, arg2: reasonCode };

    return client.revokeCert(args, (err) => {
      if (err) {
        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
      logger.debug('Certificate revoked', TAG);
      return res.status(200).json({ response: 'Certificate revoked' });
    });
  });

  app.get('/ca/:cacn/certificate/:certsn/status', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    const { cacn } = req.params;
    const { certsn } = req.params;
    const args = { arg0: cacn, arg1: certsn };

    client.checkRevokationStatus(args, (err, status) => {
      if (err) {
        logger.error('Error checking certificate status', TAG);
        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
      logger.debug('Certificate status retrieved', TAG);
      return res.status(200).json({ status });
    });
  });

  app.get('/ca/:caname/crl', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    const { caname } = req.params;

    let delta = false;

    if (req.query.delta && req.query.delta.toLowerCase() === 'true') {
      delta = true;
    }

    if (req.query.update && req.query.update.toLowerCase() === 'true') {
      // renew the CRL
      logger.debug('Renewing the CRL.', TAG);
      ejbcaUtils.crlRenew(caname);
    }

    const args = { arg0: caname, arg1: delta };

    client.getLatestCRL(args, (err, crl) => {
      if (err) {
        logger.error('Error retrieving CRL', TAG);
        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
      logger.debug('Retrieved CRL.', TAG);
      return res.status(200).json({ CRL: crl.return });
    });
  });

  app.put('/ca/:caname/crl', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    const { caname } = req.params;
    const args = { arg0: caname };

    client.createCRL(args, (err) => {
      if (err) {
        logger.error('Error creating CRL', TAG);
        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
      logger.debug('CRL Created.', TAG);

      res.location(`/ca/${caname}/crt`);
      return res.status(201).json({ response: 'CRL Created' });
    });
  });

  /* USER Routes  */

  app.post('/user', ejbcaUtils.validators.userValidator, (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    /* validate errors */
    const result = ejbcaUtils.errorValidator(req, res);
    if (result.hasError) {
      logger.error('Error validating user request', TAG);
      return res.status(400).json({
        code: 400,
        message: result.errors.toString(),
        moreinfo: config.ejbcaConf.apiURL,
      });
    }

    /* update user with default fields */
    let userData = req.body;
    userData = ejbcaUtils.updateUser(userData);

    const args = { arg0: userData };

    return client.editUser(args, (err) => {
      if (err) {
        logger.error('Error creating user', TAG);
        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
      const clientObj = { status: 0 };
      // save in cache
      return myCache.set(userData.username, clientObj, (cacheErr, success) => {
        if (!cacheErr && success) {
          logger.debug('User created.', TAG);
          return res.status(200).send('user created/edited with success.');
        }
        return res.status(500).json({
          code: 500,
          message: 'Internal',
          moreinfo: config.ejbcaConf.apiURL,
        });
      });
    });
  });

  app.get('/user/:username', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    const query = {
      matchtype: 0,
      matchvalue: req.params.username,
      matchwith: 0,
    };

    const args = { arg0: query };

    client.findUser(args, (error, user) => {
      if (error) {
        logger.error('Error finding user', TAG);
        return res.status(400).json({
          code: 400,
          message: error.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }

      if (!user) {
        return res.status(404).json({
          code: 404,
          message: 'User does not exist',
          moreinfo: config.ejbcaConf.apiURL,
        });
      }

      logger.debug('User found.', TAG);
      return res.status(200).json({ user: user.return });
    });
  });

  app.delete('/user/:username', async (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    let deleteAfter = false;

    let reasonCode = ejbcaUtils.reasons.UNSPECIFIED;

    if (req.query.reason) {
      if (req.query.reason in ejbcaUtils.reasons) {
        reasonCode = ejbcaUtils.reasons[req.query.reason];
      } else {
        logger.error('Inexistent reason', TAG);
        return res.status(404).json({
          code: 404,
          message: 'Inexistent reason code',
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
    }

    if (req.query.delete && req.query.delete.toLowerCase() === 'true') {
      deleteAfter = true;
    }

    try {
      await ejbcaUtils.deleteUser(client, req.params.username, reasonCode, deleteAfter);
    } catch (error) {
      logger.error('Error trying delete the user', TAG);
      return res.status(400).json({
        code: 400,
        message: error.err.toString(),
        moreinfo: config.ejbcaConf.apiURL,
      });
    }
    logger.debug('User deleted.', TAG);
    return res.status(200).json({ response: 'user deleted with success.' });
  });

  app.get('/user/:username/find', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    let onlyValid = true;

    if (req.query.valid && req.query.valid.toLowerCase() === 'false') {
      onlyValid = false;
    }

    const args = {
      arg0: req.params.username,
      arg1: onlyValid,
    };

    client.findCerts(args, (err, certs) => {
      if (err) {
        logger.error('Error trying to find the user', TAG);

        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }

      if (!certs) {
        logger.error("User don't have certificate", TAG);
        return res.status(404).json({
          code: 404,
          message: 'No certificate found',
          moreinfo: config.ejbcaConf.apiURL,
        });
      }


      const responseParse = {
        data: ejbcaUtils.convertCerttoX509(certs.return[0].certificateData),
      };
      logger.debug('User certificate found.', TAG);
      return res.status(200).json(responseParse);
    });
  });

  /* sign routes (for now, only pkcs10 is accepted) */

  app.post('/sign/:username/pkcs10', ejbcaUtils.validators.certificateValidator, (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');


    /* validate errors */
    const result = ejbcaUtils.errorValidator(req, res);
    if (result.hasError) {
      logger.error('Error trying to validate user', TAG);
      return res.status(400).json({
        code: 400,
        message: result.errors.toString(),
        moreinfo: config.ejbcaConf.apiURL,
      });
    }

    const { username } = req.params;
    const info = req.body;

    /* Function to send pkcs10 */
    const pkcs10Send = () => {
      const args = {
        arg0: username,
        arg1: info.passwd || 'dojot',
        arg2: info.certificate,
        arg3: null,
        arg4: 'CERTIFICATE',
      };

      client.pkcs10Request(args, (error, response) => {
        if (error) {
          logger.error('Error signing the user', TAG);
          return res.status(400).json({
            code: 400,
            message: error.toString(),
            moreinfo: config.ejbcaConf.apiURL,
          });
        }

        const responseParse = {
          data: ejbcaUtils.convertCerttoX509(response.return.data),
        };
        logger.debug('User certificate signed with success.', TAG);
        return res.status(200).json({ status: responseParse });
      });
    };

    // First we need to set the user status to new
    // (the cert can only be obtained if the user have NEW status)
    // reference: https://araschnia.unam.mx/doc/ws/index.html


    // FIX: for time improves, we cache the username and his cert status
    // (0 - Not already generated | 1- already generated)
    // here we check if the user in cache already generated the cert
    return myCache.get(username, async (err, value) => {
      if (!err) {
        if (value !== undefined && value.status === 1) {
          try {
            await ejbcaUtils.findUserandReset(client, username);

            return pkcs10Send();
          } catch (error) {
            logger.error('Error trying to find the user', TAG);
            return res.status(400).json({
              code: 400,
              message: error.toString(),
              moreinfo: config.ejbcaConf.apiURL,
            });
          }
        } else {
          const valueSet = {
            status: 1,
          };

          return myCache.set(username, valueSet, (cacheErr) => {
            if (cacheErr) {
              return res.status(500).json({
                code: 500,
                message: 'Internal error',
                moreinfo: config.ejbcaConf.apiURL,
              });
            }
            return pkcs10Send();
          });
        }
      } else {
        return res.status(500).json({
          code: 500,
          message: 'Internal error',
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
    });
  });


  /* EJBCA version */
  app.get('/ejbca/version', (req, res) => {
    res.set('Cache-Control', 'public, max-age=60');

    client.getEjbcaVersion((err, version) => {
      if (err) {
        logger.error('Error retrieving ejbca version.', TAG);
        return res.status(400).json({
          code: 400,
          message: err.toString(),
          moreinfo: config.ejbcaConf.apiURL,
        });
      }
      logger.debug('EJBCA version retrieved.', TAG);
      return res.status(200).json({ version });
    });
  });
};

module.exports = ejbcaRoute;
