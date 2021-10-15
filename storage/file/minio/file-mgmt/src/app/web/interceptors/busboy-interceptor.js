const Busboy = require('busboy');
const EventEmitter = require('events');

module.exports = (logger, minioRepositories, config) => ({
  name: 'dojot-busboy-interceptor',
  middleware: (req, res, next) => {
    const busboy = new Busboy({ headers: req.headers, limits: { files: 1, fileSize: config.minio['upload.size.limit'] } });
    const eventEmitter = new EventEmitter();
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
          res.status(413).json({ error: 'The file is too large', details: `The file size has a limit of ${config.minio['upload.size.limit']}` });
        });
        const fileinfo = await minioRepositories.putTmpObject(req.tenant, fileStream);
        req.body.uploadedFile = {
          ...fileinfo,
          filename,
          encoding,
          mimetype,
        };
        eventEmitter.emit('loaded-meta');
      } catch (error) {
        res.status(500).send({ error: 'Internal Error' });
      }
    });

    busboy.on('finish', async () => {
      logger.debug('Form upload successfully');
      eventEmitter.emit('loaded-form');
    });

    // App control auxiliary events
    eventEmitter.on('loaded-form', () => {
      loadedFile = true;
      if (loadedMeta) {
        eventEmitter.emit('loaded-end');
      }
    });

    eventEmitter.on('loaded-meta', () => {
      loadedMeta = true;
      if (loadedFile) {
        eventEmitter.emit('loaded-end');
      }
    });

    eventEmitter.on('loaded-end', async () => next());

    req.pipe(busboy);
  },
});
