const Busboy = require('busboy');
const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');

module.exports = (logger, minioRepository, config) => ({
  name: 'dojot-busboy-interceptor',
  middleware: async (req, res, next) => {
    const busboy = new Busboy({ headers: req.headers, limits: { files: 1, fileSize: config.minio['upload.size.limit'] } });
    req.body = {};
    let loadedFile = false;
    let loadedMeta = false;

    // Busboy events
    busboy.on('field', (fieldname, value) => {
      req.body[fieldname] = value;
    });

    // eslint-disable-next-line no-unused-vars
    busboy.on('file', async (fieldname, fileStream, filename, encoding, mimetype) => {
      try {
        logger.debug('Gets file stream..');
        fileStream.on('limit', () => {
          next(framework.errorTemplate.PayloadTooLarge('The file is too large', `The file size has a limit of ${config.minio['upload.size.limit']}`));
        });

        if (!(await minioRepository.bucketExists(req.tenant))) {
          logger.debug('Tenant does not exist.');
          next(framework.errorTemplate.NotFound('Tenant does not exist.', 'There is no tenancy for this tenant.'));
          return;
        }

        const fileinfo = await minioRepository.putTmpObject(req.tenant, fileStream);
        req.body.uploadedFile = {
          ...fileinfo,
          filename,
          encoding,
          mimetype,
        };
        busboy.emit('loaded-meta');
      } catch (error) {
        next(error);
      }
    });

    busboy.on('finish', () => {
      logger.debug('Form upload successfully');
      busboy.emit('loaded-form');
    });

    // App control auxiliary events
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

    busboy.on('loaded-end', () => next());

    await req.pipe(busboy);
  },
});
