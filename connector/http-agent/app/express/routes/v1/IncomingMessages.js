const { Logger } = require('@dojot/microservice-sdk');
const HttpStatus = require('http-status-codes');

const { generateDeviceDataMessage } = require('../../../Utils');
const messageSchema = require('../../schemas/messageSchema');

const logger = new Logger('http-agent:express/routes/v1/Device');

/**
 * Routes to Devices
 *
 * @param {string} mountPoint be used as a route prefix
 *
 * @param {ProducerMessages} producerMessages Instance of ProducerMessages
 */
module.exports = ({ mountPoint, producerMessages }) => [
  /**
   * Sending messages from the device
   * for a Dojot is done via HTTPS POST
   */
  {
    mountPoint,
    name: 'single-message',
    path: ['/incoming-messages', '/unsecure/incoming-messages'],
    handlers: [
      {
        method: 'post',
        middleware: [
          async (req, res) => {
            try {
              const { body } = req;

              const { error } = messageSchema.validate(body, {
                abortEarly: false,
              });
              if (error) {
                throw new Error(error.message);
              }

              await producerMessages.send(
                generateDeviceDataMessage(body, body.tenant, body.deviceId),
                body.tenant,
                body.deviceId,
              );

              res.status(HttpStatus.NO_CONTENT).send();
            } catch (e) {
              logger.error('incoming-messages.post:', e);
              res.status(HttpStatus.BAD_REQUEST).json({
                message: e.message,
              });
            }
          },
        ],
      },
    ],
  },
  {
    mountPoint,
    name: 'many-messages',
    path: [
      '/incoming-messages/create-many',
      '/unsecure/incoming-messages/create-many',
    ],
    handlers: [
      {
        method: 'post',
        middleware: [
          async (req, res) => {
            try {
              const { body } = req;
              const errors = {};

              body.forEach(async (message, index) => {
                const { error } = messageSchema.validate(
                  {
                    tenant: body.tenant,
                    deviceId: body.deviceId,
                    ...message,
                  },
                  {
                    abortEarly: false,
                  },
                );

                // eslint-disable-next-line security/detect-object-injection
                if (error) errors[index] = error.message;
              });

              if (Object.keys(errors).length) {
                throw new Error(JSON.stringify(errors));
              }

              body.forEach(async (message) => {
                await producerMessages.send(
                  generateDeviceDataMessage(
                    message,
                    body.tenant,
                    body.deviceId,
                  ),
                  body.tenant,
                  body.deviceId,
                );
              });

              res.status(HttpStatus.NO_CONTENT).send();
            } catch (e) {
              logger.error('incoming-messages.post:', e);
              res.status(HttpStatus.BAD_REQUEST).json({
                message: JSON.parse(e.message),
              });
            }
          },
        ],
      },
    ],
  },
];
