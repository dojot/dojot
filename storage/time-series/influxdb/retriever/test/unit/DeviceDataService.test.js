const DeviceDataService = require('../../app/express/services/v1/DeviceDataService');

describe('DeviceDataService', () => {
  const values = Array.from({ length: 3 }, (_, i) => i + 1);
  const deviceDataRepository = {
    async queryByMeasurement() {
      return { result: 'result', totalItems: 0 };
    },
    async queryByField() {
      return { result: 'result', totalItems: 0 };
    },
  };
  let deviceDataService;

  beforeEach(() => {
    deviceDataService = new DeviceDataService(deviceDataRepository);
  });

  test('should return all device data', async () => {
    const returnData = await deviceDataService.getDeviceData('test', 'test', '', '', 10, 0, 'asc', () => ({ current: 'url' }));
    expect(returnData).toEqual([
      'result',
      { current: 'url' },
    ]);
  });

  test('should return only selected attributes', async () => {
    const returnData = await deviceDataService.getDeviceAttrData('test', 'test', 'test', '', '', 10, 0, 'asc', () => ({ current: 'url' }));
    expect(returnData).toEqual([
      'result',
      { current: 'url' },
    ]);
  });

  test('should transform device data object to a spreadsheet in csv', () => {
    const deviceData = [
      {
        ts: '2021-07-28T11:47:53.365Z',
        attrs: values.map((value) => ({ label: `column${value}`, value: value * 10 })),
      },
      {
        ts: '2021-07-29T11:47:53.365Z',
        attrs: values.map((value) => ({ label: `column${value}`, value: value * 10 })),
      },
    ];

    const correctValue = '"ts","column1","column2","column3"\n"2021-07-28T11:47:53.365Z",10,20,30\n"2021-07-29T11:47:53.365Z",10,20,30';

    const sheet = DeviceDataService.parseDeviceDataToCsv(deviceData);
    expect(sheet).toBe(correctValue);
  });

  test('should transform the device data object into a csv spreadsheet with blank spaces', () => {
    const deviceData = [
      {
        ts: '2021-07-28T11:47:53.365Z',
        attrs: values.map((value) => ({ label: `column${value}`, value: value * 10 })),
      },
      {
        ts: '2021-07-29T11:47:53.365Z',
        attrs: values.filter((value) => value !== 2).map((value) => ({ label: `column${value}`, value: value * 10 })),
      },
    ];

    const correctValue = '"ts","column1","column2","column3"\n"2021-07-28T11:47:53.365Z",10,20,30\n"2021-07-29T11:47:53.365Z",10,,30';
    const sheet = DeviceDataService.parseDeviceDataToCsv(deviceData);
    expect(sheet).toBe(correctValue);
  });

  test('should transform device data object to empty string, when device data object is empty', () => {
    const deviceData = [];

    const correctValue = '';

    const sheet = DeviceDataService.parseDeviceDataToCsv(deviceData);
    expect(sheet).toBe(correctValue);
  });
});
