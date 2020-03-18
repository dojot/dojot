const utils = require('../../app/utils/utils');

describe('Testing utils', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should generate correctly the payload', () => {
    const topic = 'admin:deviceid/topic';
    const payload = 'data';
    const data = utils.generateDojotDeviceDataMessage(topic, payload);

    const { deviceid } = data.metadata;
    const { tenant } = data.metadata;
    const { attrs } = data;

    expect(deviceid).toEqual('deviceid');
    expect(tenant).toEqual('admin');
    expect(attrs).toEqual(payload);
  });
});
