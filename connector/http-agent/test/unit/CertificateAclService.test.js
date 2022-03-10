const mockAxios = {
  default: {
    create: jest.fn(() => ({
      // eslint-disable-next-line no-unused-vars
      get: jest.fn((url, options) => {
        const baseURL = url.split('/');
        if (baseURL[0] !== 'null') {
          return {
            data: 'test:abc123',
          };
        }

        throw new Error('Error');
      }),
    })),
  },
};

jest.mock('axios', () => mockAxios);

const axios = require('../../app/axios/createAxios');
const CertificateAclService = require('../../app/axios/CertificateAclService');


describe('CertificateAclService', () => {
  let certificateAclService;

  it('should return teant and device id', async () => {
    certificateAclService = new CertificateAclService('apicertificateAcl', axios);
    const messageKey = await certificateAclService.getAclEntries('A1:B2:C3:D4:E5:F6:G7');

    expect(messageKey).toEqual('test:abc123');
  });

  it('should throw a error, when the request failed', async () => {
    let error;
    certificateAclService = new CertificateAclService(null, axios);
    try {
      await certificateAclService.getAclEntries();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
