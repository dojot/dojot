const moment = require('moment');
const { generateDeviceDataMessage } = require('../../app/Utils');

const tenant = 'test';
const deviceid = 'abc123';
const fakeMessageWithUTC = {
  ts: '2021-07-12T09:31:01.683000Z',
  data: {
    temperature: 25.79,
  },
};
const fakeMessageWithUnix = {
  ts: '1636513779579',
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

describe('Util', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should format the message correctly with UTC', async () => {
    const deviceDataMessage = generateDeviceDataMessage(
      fakeMessageWithUTC,
      tenant,
      deviceid,
    );

    const formattedMessage = {
      metadata: {
        deviceid,
        tenant,
        timestamp: moment(fakeMessageWithUTC.ts).valueOf(),
      },
      attrs: fakeMessageWithUTC.data,
    };

    expect(JSON.stringify(deviceDataMessage)).toEqual(
      JSON.stringify(formattedMessage),
    );
  });

  it('should format the message correctly with unix', async () => {
    const deviceDataMessage = generateDeviceDataMessage(
      fakeMessageWithUnix,
      tenant,
      deviceid,
    );

    const formattedMessage = {
      metadata: {
        deviceid,
        tenant,
        timestamp: moment(fakeMessageWithUnix.ts, 'X', true).valueOf(),
      },
      attrs: fakeMessageWithUnix.data,
    };

    expect(JSON.stringify(deviceDataMessage)).toEqual(
      JSON.stringify(formattedMessage),
    );
  });

  it('should format the message correctly with invalid timestamp', async () => {
    let err;
    try {
      generateDeviceDataMessage(
        fakeMessageWithInvalidTimestamp,
        tenant,
        deviceid,
      );
    } catch (e) {
      err = e;
    }

    expect(err.message).toEqual('Invalid timestamp');
  });
});
