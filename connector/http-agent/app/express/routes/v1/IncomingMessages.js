const { Logger } = require('@dojot/microservice-sdk');
const HttpStatus = require('http-status-codes');

const { generateDeviceDataMessage } = require('../../../Utils');

const logger = new Logger('http-agent:express/routes/v1/Device');

const createSchemaValidator = require('../../helpers/schemaValidator');

const multipleMessagesSchema = require('../../schemas/multiple-messages.json');
const singleMessageSchema = require('../../schemas/single-message.json');

const validators = createSchemaValidator(
  {
    schemas: {
      multipleMessagesSchema,
      singleMessageSchema,
    },
  },
);
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
          validators.validateSingleMessage(),
          async (req, res) => {
            try {
              const { body } = req;

              await producerMessages.send(
                generateDeviceDataMessage(body, body.tenant, body.deviceId),
                body.tenant,
                body.deviceId,
              );

              res.status(HttpStatus.NO_CONTENT).send();
            } catch (e) {
              logger.error('incoming-messages.post:', e);
              res.status(HttpStatus.StatusCodes.FAILED_DEPENDENCY).json({
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
    name: 'many-messages',
    path: [
      '/incoming-messages/create-many',
      '/unsecure/incoming-messages/create-many',
    ],
    handlers: [
      {
        method: 'post',
        middleware: [
          validators.validateMultipleMessages(),
          async (req, res) => {
            try {
              const { body } = req;
              const errors = {};

              const payloadsPromises = body.map(async (message, index) => {
                try {
                  await producerMessages.send(
                    generateDeviceDataMessage(
                      message,
                      body.tenant,
                      body.deviceId,
                    ),
                    body.tenant,
                    body.deviceId,
                  );
                } catch (e) {
                  errors[+index] = e.message;
                }
              });

              await Promise.all(payloadsPromises);

              if (Object.keys(errors).length) {
                throw new Error(JSON.stringify(errors));
              }

              res.status(HttpStatus.NO_CONTENT).send();
            } catch (e) {
              logger.error('incoming-messages.post:', e);
              res.status(HttpStatus.StatusCodes.FAILED_DEPENDENCY).json({
                error: JSON.parse(e.message),
              });
            }
          },
        ],
      },
    ],
  },
];
