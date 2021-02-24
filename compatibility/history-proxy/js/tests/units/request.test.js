const supertest = require('supertest');
const axios = require('axios');
const app = require('../src/index');

const AUTH_TOKEN = 'Bearer eyJ0eXAiOiJC1QiLCJhbGciOiJIS1NiJ9.eyJpc3MiOiJDWmwzVnRucWpWMmFIdGtXU29iMGJWU1lveGRGNFFCMCIsImlhdCI6MTYwODUxNzg1NCwiZXhwIjoxNjA3NTE4Mjc0LCJwcm9maWxlIjoiYWRtaW4iLCJnc21cHMiOlsxXSwidXNlcmlkIRCJqdGkiOiI1ZjY3YWZhN2Q2M2YyOWFlM2I3Y2VmYzUwODBlMmRlYiIsInNlcnZpY2UiOiJhZG1pbiIsInVzZXJuYW1lIjoicWRtaW4ifQ.hdljpZ7cIsRTI5x-43uWWzKprjWcFF9CCg1dCxLK9QA';

jest.mock('axios');

const caseOne = {
  request: {
    attr: 'temperature',
    dateTo: null,
    dateFrom: null,
    lastN: 0,
  },
  expected:
    [{
      attr: 'temperature', device_id: 'a1b1c1', metadata: {}, ts: '2021-02-13T15:12:09.579Z',
    }, {
      attr: 'temperature', device_id: 'a1b1c1', metadata: {}, ts: '2021-02-13T15:13:09.579Z',
    }],
};

const caseTwo = {
  request: {
    dateTo: null,
    dateFrom: null,
    lastN: 0,
  },
  expected:
  {
    title: 'Missing data',
    description: 'No attribute was passed.',
  },
};

describe('Unit testing to validate routes', () => {
  const fetchData = async (deviceId, currentCase) => {
    const paramList = currentCase.request;

    const pms = new URLSearchParams(paramList);

    return supertest(app)
      .get(`/history/device/${deviceId}/history?${pms.toString()}`)
      .set('Authorization', AUTH_TOKEN)
      .set('Accept', 'application/json');
  };

  // mocking influx response
  axios.get.mockImplementation((url, headers) => {
    expect(headers.headers.authorization).toEqual(AUTH_TOKEN);
    return Promise.resolve({
      data: {
        data: [
          {
            ts: '2021-02-13T15:12:09.579Z',
            attrs: [
              {
                label: 'temperature',
                value: '10',
              }],
          },
          {
            ts: '2021-02-13T15:13:09.579Z',
            attrs: [
              {
                label: 'temperature',
                value: '14',
              }],
          },
        ],
        paging: {
        },
      },
    });
  });

  it('checking if errors in been sent up', async (done) => {
    const deviceId = 'a1b1c1';
    const response = await fetchData(deviceId, caseTwo);
    expect(response.statusCode).toEqual(400);
    expect(response.body).toEqual(caseTwo.expected);
    done();
  });

  it('checking if header was been passed', async (done) => {
    const deviceId = 'a1b1c1';
    const response = await fetchData(deviceId, caseOne);
    expect(response.statusCode).toEqual(200);
    done();
  });

  it('checking if the correct data manipulation', async (done) => {
    const deviceId = 'a1b1c1';
    const response = await fetchData(deviceId, caseOne);
    expect(response.body).toEqual(caseOne.expected);
    done();
  });

  afterAll(async () => {
    await new Promise((resolve) => setTimeout(() => resolve(), 500));
  });
});
