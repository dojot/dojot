const GenericQueryService = require('../../app/express/services/v1/GenericQueryService');

describe('DeviceDataService', () => {
  const deviceDataRepository = {
    async runGenericQuery() {
      return [
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
      ];
    },
    async runGenericFlexQuery() {
      return [
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
      ];
    },
  };
  let genericQueryService;

  beforeEach(() => {
    genericQueryService = new GenericQueryService(deviceDataRepository);
  });

  test('should execute query and return data', async () => {
    const returnData = await genericQueryService.runQuery('default', 'query');
    expect(returnData).toEqual([
      {
        result: '_result',
        table: 0,
        _value: '_value',
      },
    ]);
  });

  test('should throw an error, when the query has a from operation', async () => {
    expect.assertions(1);
    try {
      await genericQueryService.runQuery('default', 'from(bucket:"default")');
    } catch (error) {
      expect(error.responseJSON.error).toEqual('The "from" function is determined by dojot');
    }
  });

  test('should execute flex query and return data', async () => {
    const returnData = await genericQueryService.runFlexQuery('default', 'flexquery');
    expect(returnData).toEqual([
      {
        result: '_result',
        table: 0,
        _value: '_value',
      },
    ]);
  });
});
