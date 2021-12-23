const { Logger } = require('@dojot/microservice-sdk');
const HttpStatus = require('http-status-codes');

const { generateDeviceDataMessage } = require('../../../Utils');
const messageSchema = require('../../schemas/messageSchema');

const logger = new Logger('http-agent:express/routes/v1/Device');

/**
 * Routes to Devices
 *
 * @param {string} mountPoint be used as a route prefix
 * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByField
 *                               A promise that returns a result and a totalItems inside that result
 * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByMeasurement
 *                               A promise that returns a result and a totalItems inside that result
 */
module.exports = ({ mountPoint, producerMessages }) => {
  /**
   * Sending messages from the device
   * for a Dojot is done via HTTPS POST
   */
  const routes = [
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
                const { body, tenant, deviceId } = req;

                const generatedDeviceDataMessage = generateDeviceDataMessage(
                  body,
                  tenant,
                  deviceId,
                );

                const { error } = messageSchema.validate(generatedDeviceDataMessage,
                  {
                    abortEarly: false,
                  });
                if (error) {
                  throw new Error(error.message);
                }

                await producerMessages.send(
                  generatedDeviceDataMessage,
                  tenant,
                  deviceId,
                );

                res.status(HttpStatus.OK).json({
                  success: true,
                  message: 'Successfully published!',
                });
              } catch (e) {
                logger.error('incoming-messages.post:', e);
                res.status(HttpStatus.BAD_REQUEST).json({
                  success: false,
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
                const { body, tenant, deviceId } = req;
                const errors = {};

                body.forEach(async (message, index) => {
                  const generatedDeviceDataMessage = generateDeviceDataMessage(
                    message,
                    tenant,
                    deviceId,
                  );

                  const { error } = messageSchema.validate(generatedDeviceDataMessage,
                    {
                      abortEarly: false,
                    });

                  // eslint-disable-next-line security/detect-object-injection
                  if (error) errors[index] = error.message;

                  await producerMessages.send(
                    generatedDeviceDataMessage,
                    tenant,
                    deviceId,
                  );
                });

                if (Object.keys(errors).length) {
                  throw new Error(JSON.stringify(errors));
                }

                res.status(HttpStatus.OK).json({
                  success: true,
                  message: 'Successfully published!',
                });
              } catch (e) {
                logger.error('incoming-messages.post:', e);
                res.status(HttpStatus.BAD_REQUEST).json({
                  success: false,
                  message: JSON.parse(e.message),
                });
              }
            },
          ],
        },
      ],
    },
  ];

  return routes;
};
