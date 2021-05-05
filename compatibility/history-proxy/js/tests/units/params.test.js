const mockConfig = {
  'server': { 'retriever': { 'protocol': 'https' } },
};

const mockSdk = {
  ConfigManager: {
    loadSettings: jest.fn(),
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);


const { createDataToBePassed } = require('../../src/proxy');

const validationHandler = require('../../src/handlers/handleValidation');

const expectedValue = {
  deviceId: 'a1b1c1',
  attr: 'temperature',
  dateFrom: '1970-01-01T00:00:00.000Z',
  dateTo: (new Date().toISOString()).split('.')[0],
  order: 'desc',
  isAllAttrs: false,
  isMultipleAttr: false,
  headers: undefined,
  rawResponse: '',
  limit: 256,
};

const cases = {
  caseOne: {
    request: {
      headers: {},
      params: {
        deviceId: 'a1b1c1',
      },
      query: {
        attr: 'temperature',
      },
    },
    expected:
    {
      ...expectedValue,
    },
  },
  caseTwo: {
    request: {
      headers: {},
      params: {
        deviceId: 'a1b1c1',
      },
      query: {
        attr: 'temperature',
        lastN: 100,
      },
    },
    expected:
    {
      ...expectedValue,
      limit: 100,
      order: 'desc',
    },
  },
  caseThree: {
    request: {
      headers: {},
      params: {
        deviceId: 'a1b1c1',
      },
      query: {
        attr: 'temperature',
        lastN: 200,
        dateFrom: '2021-01-01T09:00:00.000Z',
      },
    },
    expected:
    {
      ...expectedValue,
      limit: 200,
      order: 'desc',
      dateFrom: '2021-01-01T09:00:00.000Z',
    },
  },
  caseFour: {
    request: {
      headers: {},
      params: {
        deviceId: 'a1b1c1',
      },
      query: {
        attr: 'temperature',
        firstN: 50,
        dateTo: '2021-02-02T12:00:00.000Z'.split('.')[0],
        dateFrom: '2021-01-01T09:00:00.000Z',
      },
    },
    expected:
    {
      ...expectedValue,
      limit: 50,
      order: 'asc',
      dateTo: '2021-02-02T12:00:00.000Z'.split('.')[0],
      dateFrom: '2021-01-01T09:00:00.000Z',
    },
  },
  caseFive: {
    request: {
      headers: {},
      params: {
        deviceId: 'a1b1c1',
      },
      query: {
        firstN: 0,
        attr: ['temperature', 'temp2'],
      },
    },
    expected:
    {
      ...expectedValue,
      isMultipleAttr: true,
    },
  },
  caseSix: {
    request: {
      headers: {},
      params: {
        deviceId: 'a1b1c1',
      },
      query: {
      },
    },
    expected:
    {
      ...expectedValue,
      attr: undefined,
      isAllAttrs: true,
    },
  },
};

describe('Testing incoming params and outcoming params', () => {
  it('pass only one attribute', async () => {
    const paramList = cases.caseOne.request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases.caseOne.expected);
  });

  it('pass attribute and lastN', async () => {
    const paramList = cases.caseTwo.request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases.caseTwo.expected);
  });

  it('passing dateTo and lastN', async () => {
    const paramList = cases.caseThree.request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking

    expect({ ...result, dateTo }).toEqual(cases.caseThree.expected);
  });

  it('passing dateTo, dateFrom and firstN', async () => {
    const paramList = cases.caseFour.request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases.caseFour.expected);
  });

  it('passing 2 attributes', async () => {
    const paramList = cases.caseFive.request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases.caseFive.expected);
  });

  it('checking all attrs (no send attributes)', async () => {
    const paramList = cases.caseSix.request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases.caseSix.expected);
  });
});
