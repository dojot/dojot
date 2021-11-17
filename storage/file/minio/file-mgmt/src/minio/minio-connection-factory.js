const Minio = require('minio');
const { promisify } = require('util');

/**
 * Creates the MinIo client.
 *
 * @param {*} configMinio the minio settings
 *
 * @returns MinIo Client
 */
module.exports = (configMinio) => {
  const minioClient = new Minio.Client({
    endPoint: configMinio.host,
    port: configMinio.port,
    useSSL: configMinio.ssl,
    accessKey: configMinio['access.key'],
    secretKey: configMinio['secret.key'],
  });

  // Promissify callback-based methods
  minioClient.makeBucket = promisify(minioClient.makeBucket);
  minioClient.removeBucket = promisify(minioClient.removeBucket);
  minioClient.bucketExists = promisify(minioClient.bucketExists);
  minioClient.putObject = promisify(minioClient.putObject);
  minioClient.removeObject = promisify(minioClient.removeObject);
  minioClient.copyObject = promisify(minioClient.copyObject);
  minioClient.statObject = promisify(minioClient.statObject);
  minioClient.getObject = promisify(minioClient.getObject);
  minioClient.presignedGetObject = promisify(minioClient.presignedGetObject);
  minioClient.removeObjects = promisify(minioClient.removeObjects);

  return minioClient;
};
