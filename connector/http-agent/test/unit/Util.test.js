const { generateDeviceDataMessage, sslCADecode } = require('../../app/Utils');

const tenant = 'test';
const deviceid = 'abc123';
const fakeMessageWithTimestamp = {
  ts: '2021-07-12T09:31:01.683000Z',
  data: {
    temperature: 25.79,
  },
};
const fakeMessageWithUnixTimestamp = {
  ts: '1626082261683',
  data: {
    temperature: 25.79,
  },
};
const fakeMessageWithoutTimestamp = {
  data: {
    temperature: 25.79,
  },
};
const fakeMessageWithInvalidTimestamp = {
  ts: 'invalid',
  data: {
    temperature: 25.79,
  },
};
const fakeListCAsString = '-----BEGIN CERTIFICATE-----CA_CONTENT_1-----END CERTIFICATE-----\n-----BEGIN CERTIFICATE-----CA_CONTENT_2-----END CERTIFICATE-----';

describe('Util', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should format CA correctly with line break separation', async () => {
    const sslCAData = sslCADecode(fakeListCAsString);

    const formatedSSLCAString = `-----BEGIN CERTIFICATE-----CA_CONTENT_1-----END CERTIFICATE-----
-----BEGIN CERTIFICATE-----CA_CONTENT_2-----END CERTIFICATE-----
`;

    expect(sslCAData).toEqual(
      formatedSSLCAString,
    );
  });

  it('should format the message correctly with timestamp', async () => {
    const deviceDataMessage = generateDeviceDataMessage(
      fakeMessageWithTimestamp,
      tenant,
      deviceid,
    );

    const formattedMessage = {
      metadata: {
        deviceid,
        tenant,
        timestamp: Date.parse(fakeMessageWithTimestamp.ts),
      },
      attrs: fakeMessageWithTimestamp.data,
    };

    expect(JSON.stringify(deviceDataMessage)).toEqual(
      JSON.stringify(formattedMessage),
    );
  });

  it('should format the message correctly with unix timestamp', async () => {
    const deviceDataMessage = generateDeviceDataMessage(
      fakeMessageWithUnixTimestamp,
      tenant,
      deviceid,
    );

    const formattedMessage = {
      metadata: {
        deviceid,
        tenant,
        timestamp: Date.parse(fakeMessageWithTimestamp.ts),
      },
      attrs: fakeMessageWithTimestamp.data,
    };

    expect(JSON.stringify(deviceDataMessage)).toEqual(
      JSON.stringify(formattedMessage),
    );
  });

  it('should format the message correctly without timestamp', async () => {
    const deviceDataMessage = generateDeviceDataMessage(
      fakeMessageWithoutTimestamp,
      tenant,
      deviceid,
    );

    const formattedMessage = {
      metadata: {
        deviceid,
        tenant,
        timestamp: new Date().getTime(),
      },
      attrs: fakeMessageWithoutTimestamp.data,
    };

    expect(JSON.stringify(deviceDataMessage)).toEqual(
      JSON.stringify(formattedMessage),
    );
  });

  it('should format the message correctly with invalid timestamp', async () => {
    let error;
    try {
      generateDeviceDataMessage(
        fakeMessageWithInvalidTimestamp,
        tenant,
        deviceid,
      );
    } catch (err) {
      error = err;
    }
    expect(error.message).toEqual('Invalid timestamp');
  });
});
