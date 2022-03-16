const configMinio = {
  host: 'test',
  port: 9000,
  useSSL: false,
  accessKey: 'accessKey',
  secretKey: 'secretKey',
};

const createMinIOConnection = require('../../../src/minio/minio-connection-factory');

describe('MinIoConnectionFactory', () => {
  it('Should make a MinIo connection', () => {
    const connection = createMinIOConnection(configMinio);
    expect(connection).toBeDefined();
  });

  it('Should throw an error, when there is an error', () => {
    let error;
    try {
      createMinIOConnection({});
    } catch (e) {
      error = e;
    }

    expect(error).toBeDefined();
  });
});
