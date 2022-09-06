const mockDojotHttpClient = {
  request: jest.fn(() => ({ data: 'test:abc123' })),
};

const CertificateAclService = require('../../app/axios/CertificateAclService');

describe('CertificateAclService', () => {
  let certificateAclService;

  it('should return teant and device id', async () => {
    certificateAclService = new CertificateAclService('apicertificateAcl', mockDojotHttpClient);
    const messageKey = await certificateAclService.getAclEntries('A1:B2:C3:D4:E5:F6:G7');

    expect(messageKey).toEqual('test:abc123');
  });

  it('should throw a error, when the request failed', async () => {
    mockDojotHttpClient.request.mockRejectedValueOnce(new Error('Error'));
    let error;
    certificateAclService = new CertificateAclService(null, mockDojotHttpClient);
    try {
      await certificateAclService.getAclEntries();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
