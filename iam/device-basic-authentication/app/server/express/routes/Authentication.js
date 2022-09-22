const { Logger } = require('@dojot/microservice-sdk');
const HttpStatus = require('http-status-codes');

const { generateMessage } = require('../../../Utils');

const logger = new Logger('basic-auth:authentication');

/**
 * Routes to Devices
 *
 * @param {string} mountPoint be used as a route prefix
 *
 * @param {ProducerMessages} producerMessages Instance of ProducerMessages
 *
 * @param {BasicCredentials} basicCredentialsCtrl Instance of BasicCredentials
 */
module.exports = ({
  mountPoint, producerMessages, basicCredentialsCtrl,
}) => (
  /**
   * Sending messages from the device
   * for a Dojot is done via HTTPS POST
   */
  [
    {
      mountPoint,
      name: 'generate-credentials',
      path: ['/devices/:deviceId/basic-credentials'],
      handlers: [
        {
          method: 'post',
          middleware: [
            async (req, res) => {
              try {
                const { tenant, params: { deviceId } } = req;

                const credentials = await basicCredentialsCtrl.create(tenant.id, deviceId);

                await producerMessages.send(
                  generateMessage(tenant.id, deviceId),
                  tenant.id,
                  deviceId,
                );
                const buff = Buffer.from(`${tenant.id}@${deviceId}:${credentials.password}`, 'utf-8');
                const basicAuth = `Basic ${buff.toString('base64')}`;

                return res.status(HttpStatus.OK).json({ credentials, basicAuth });
              } catch (e) {
                logger.error('incoming-messages.post:', e);
                return res.status(HttpStatus.INTERNAL_SERVER_ERROR).json({
                  error: e.message,
                });
              }
            },
          ],
        },
      ],
    },
    {
      mountPoint,
      name: 'authenticate-credentials',
      path: [
        '/internal/authentication',
      ],
      handlers: [
        {
          method: 'post',
          middleware: [
            async (req, res) => {
              try {
                const { username, password } = req.body;
                let tenant;
                let deviceId;

                try {
                  [tenant, deviceId] = username.split('@');
                } catch (err) {
                  return res.status(HttpStatus.UNAUTHORIZED).json({
                    message: 'The credential is invalid.',
                  });
                }

                const isMatch = await basicCredentialsCtrl.authentication(
                  tenant,
                  deviceId,
                  password,
                );

                if (!isMatch) {
                  return res.status(HttpStatus.UNAUTHORIZED).json({
                    message: 'The credential is invalid.',
                  });
                }
                return res.status(HttpStatus.OK).json({
                  message: 'The credential is valid.',
                });
              } catch (e) {
                logger.error('incoming-messages.post:', e);
                return res.status(HttpStatus.INTERNAL_SERVER_ERROR).json({
                  error: e.message,
                });
              }
            },
          ],
        },
      ],
    },
  ]
);
