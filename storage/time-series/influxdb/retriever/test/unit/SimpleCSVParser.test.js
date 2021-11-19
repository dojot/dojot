const { parseCSV } = require('../../app/express/helpers/SimpleCSVParser');

describe('SimpleCSVParser', () => {
  test('should transform the device attr data object to a spreadsheet in csv', () => {
    const deviceData = [
      {
        ts: '2021-07-28T11:47:53.365Z',
        value: 10,
      },
      {
        ts: '2021-07-29T11:47:53.365Z',
        value: 20,
      },
    ];

    const correctValue = '"ts","value"\n"2021-07-28T11:47:53.365Z",10\n"2021-07-29T11:47:53.365Z",20';
    const sheet = parseCSV(deviceData);
    expect(sheet).toBe(correctValue);
  });

  test('should transform the device attr data object to empty string, when device attr data object is empty', () => {
    const deviceData = [];

    const correctValue = '';
    const sheet = parseCSV(deviceData);
    expect(sheet).toBe(correctValue);
  });
});
