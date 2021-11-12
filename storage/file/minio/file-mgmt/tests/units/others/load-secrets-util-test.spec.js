const mockFs = {
  promises: {
    readFile: jest.fn(),
  },
};
jest.mock('fs', () => mockFs);

const LoadSecretsUtil = require('../../../src/utils/load-secrets-util');

describe('LoadSecretsUtil', () => {
  it('Should set access key ', async () => {
    mockFs.promises.readFile.mockReturnValueOnce('access_key');
    const config = {
      minio: {
        'access.key.file': 'TEST_TEST',
        'secret.key': 'secret_key',
      },
    };
    await LoadSecretsUtil.loadSecrets(config);
    
    expect(config.minio['access.key']).toEqual('access_key');
    expect(config.minio['secret.key']).toEqual('secret_key');
  });

  it('Should set secret key ', async () => {
    mockFs.promises.readFile.mockReturnValueOnce('secret_key');
    const config = {
      minio: {
        'secret.key.file': 'TEST_TEST',
        'access.key': 'access_key',
      },
    };
    await LoadSecretsUtil.loadSecrets(config);
    
    expect(config.minio['access.key']).toEqual('access_key');
    expect(config.minio['secret.key']).toEqual('secret_key');
  });
});
