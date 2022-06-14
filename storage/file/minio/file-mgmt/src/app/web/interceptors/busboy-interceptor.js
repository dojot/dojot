const Busboy = require('busboy');
const { v4: uuidv4 } = require('uuid');

const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');


/**
 * Makes an instance of busboy interceptor.
 *
 * @param {Logger} logger Dojot Logger
 * @param {MinIoRepository} minioRepository MinIo Repository
 * @param {Config} config Application settings
 *
 * @returns an instance of busboy interceptor
 */
module.exports = (logger, minioRepository, config) => ({
  name: 'dojot-busboy-interceptor',
  middleware: async (req, res, next) => {
    try {
      if (!req.is('multipart/form-data')) {
        next(framework.errorTemplate.UnsupportedMediaType('Unsupported Media Type.', 'Unsupported Media Type.'));
        return;
      }

      const busboy = new Busboy({ headers: req.headers, limits: { files: 1, fileSize: Number(config.minio['upload.size.limit']) } });
      req.body = {};
      let loadedFile = false;
      let loadedMeta = false;

      // Gets the fields
      busboy.on('field', (fieldname, value) => {
        if (fieldname === 'file') {
          next(framework.errorTemplate.BadRequest('File was not informed.', 'File was not informed correctly.'));
        }
        req.body[`${fieldname}`] = value;
      });

      // Gets the file
      // eslint-disable-next-line no-unused-vars
      busboy.on('file', async (fieldname, fileStream, filename, encoding, mimetype) => {
        try {
          const transactionCode = uuidv4();
          logger.debug('Gets file stream..');

          // If the file exceeds the size limit
          fileStream.on('limit', async () => {
            await minioRepository.rollbackObject(req.tenant, transactionCode);
            next(framework.errorTemplate.PayloadTooLarge('The file is too large', `The file exceeds the maximum size of ${config.minio['upload.size.limit']}`));
          });

          if (!(await minioRepository.bucketExists(req.tenant))) {
            logger.debug('Tenant does not exist.');
            next(framework.errorTemplate.BadRequest('Tenant does not exist.', 'There is no bucket for this tenant.'));
            return;
          }

          // Initialize a transaction
          const fileinfo = await minioRepository.putTmpObject(
            req.tenant, fileStream, transactionCode,
          );
          req.body.uploadedFile = {
            ...fileinfo,
            filename,
            encoding,
            mimetype,
          };

          // Emits the signal that the transaction metadata has been loaded
          busboy.emit('loaded-meta');
        } catch (error) {
          next(error);
        }
      });

      // When the file has finished loading, it emits a signal that the form has been loaded.
      busboy.on('finish', () => {
        logger.debug('Form upload successfully');
        busboy.emit('loaded-form');
      });

      // Checks if the file and its metadata have been loaded
      busboy.on('loaded-form', () => {
        loadedFile = true;
        if (loadedMeta) {
          busboy.emit('loaded-end');
        }
      });

      busboy.on('loaded-meta', () => {
        loadedMeta = true;
        if (loadedFile) {
          busboy.emit('loaded-end');
        }
      });

      // If that's ok.
      busboy.on('loaded-end', () => next());

      await req.pipe(busboy);
    } catch (error) {
      next(framework.errorTemplate.BadRequest('The request headers invalid', error.message));
    }
  },
});
