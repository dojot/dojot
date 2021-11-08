const { generateMessage } = require('../../app/Utils');

const tenant = 'test';
const deviceId = 'abc123';

describe('Util', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should format the message correctly', async () => {
    const deviceDataMessage = generateMessage('create', tenant, deviceId);

    const formattedMessage = {
      event: 'create',
      meta: {
        service: 'test',
      },
      data: {
        id: 'abc123',
      },
    };

    expect(JSON.stringify(deviceDataMessage)).toEqual(
      JSON.stringify(formattedMessage),
    );
  });
});
