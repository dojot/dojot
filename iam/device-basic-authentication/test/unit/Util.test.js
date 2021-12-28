const { generateMessage } = require('../../app/Utils');

const tenant = 'test';
const device = 'abc123';

describe('Util', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should format the message correctly', async () => {
    const dataMessage = generateMessage(tenant, device);

    const formattedData = {
      opr: 'create',
      tenant,
      device,
    };

    expect(dataMessage.metadata.msgid).toBeDefined();
    expect(dataMessage.metadata.ts).toBeDefined();
    expect(dataMessage.metadata.service).toEqual('basic-auth');
    expect(dataMessage.metadata.content_type).toEqual('application/vnd.dojot.devices.basic-credentials+json');
    expect(JSON.stringify(dataMessage.data)).toEqual(JSON.stringify(formattedData));
  });
});
