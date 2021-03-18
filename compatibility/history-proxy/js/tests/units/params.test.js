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

const CASE_ONE = 'caseOne';
const CASE_TWO = 'caseTwo';
const CASE_THREE = 'caseThree';
const CASE_FOUR = 'caseFour';
const CASE_FIVE = 'caseFive';
const CASE_SIX = 'caseSix';

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
    const paramList = cases[CASE_ONE].request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases[CASE_ONE].expected);
  });

  it('pass attribute and lastN', async () => {
    const paramList = cases[CASE_TWO].request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases[CASE_TWO].expected);
  });

  it('passing dateTo and lastN', async () => {
    const paramList = cases[CASE_THREE].request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking

    expect({ ...result, dateTo }).toEqual(cases[CASE_THREE].expected);
  });

  it('passing dateTo, dateFrom and firstN', async () => {
    const paramList = cases[CASE_FOUR].request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases[CASE_FOUR].expected);
  });

  it('passing 2 attributes', async () => {
    const paramList = cases[CASE_FIVE].request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases[CASE_FIVE].expected);
  });

  it('checking all attrs (no send attributes)', async () => {
    const paramList = cases[CASE_SIX].request;
    const data = createDataToBePassed(paramList);
    const result = await validationHandler.handle(data);
    const dateTo = result.dateTo.split('.')[0]; // removing ms for checking
    expect({ ...result, dateTo }).toEqual(cases[CASE_SIX].expected);
  });
});
