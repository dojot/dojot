const Busboy = require('busboy');
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
    const busboy = new Busboy({ headers: req.headers, limits: { files: 1, fileSize: config.minio['upload.size.limit'] } });
    req.body = {};
    let loadedFile = false;
    let loadedMeta = false;

    // Gets the fields
    busboy.on('field', (fieldname, value) => {
      req.body[`${fieldname}`] = value;
    });

    // Gets the file
    // eslint-disable-next-line no-unused-vars
    busboy.on('file', async (fieldname, fileStream, filename, encoding, mimetype) => {
      try {
        logger.debug('Gets file stream..');
        // If the file exceeds the size limit
        fileStream.on('limit', () => {
          next(framework.errorTemplate.PayloadTooLarge('The file is too large', `The file exceeds the maximum size of ${config.minio['upload.size.limit']}`));
        });

        if (!(await minioRepository.bucketExists(req.tenant.id))) {
          logger.debug('Tenant does not exist.');
          next(framework.errorTemplate.BadRequest('Tenant does not exist.', 'There is no bucket for this tenant.'));
          return;
        }

        // Initialize a transaction
        const fileinfo = await minioRepository.putTmpObject(req.tenant.id, fileStream);
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
  },
});
