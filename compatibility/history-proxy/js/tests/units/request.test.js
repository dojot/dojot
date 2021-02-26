const supertest = require('supertest');


// mocking influx response
const fakeInfluxResponse =
{
  "temperature": {
    data: [
      {
        ts: '2021-02-13T15:12:10.579Z',
        label: 'temperature',
        value: '10',
      },
      {
        ts: '2021-02-13T15:13:09.579Z',
        label: 'temperature',
        value: '9',
      },
    ],
    paging: {
    },
  },
  "unknown_attribute": {
    data: [
    ]
  },
  "all_attributes": {
    data: [
      {
        ts: '2021-02-13T15:12:10.579Z',
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
            value: '9',
          }],
      },
    ],
    paging: {
    },
  },
};

jest.mock('./../../src/handlers/utils', () => {
  return {
    callInflux: jest.fn((options) => {
      const attr = options.path.split('/')[6];
      let data = fakeInfluxResponse[attr];
      if (!attr)
        data = fakeInfluxResponse['all_attributes'];
      return new Promise((resolve, reject) => {
        resolve(data);
      });
    })
  }
});

const { framework } = require('../../src/routes');

const request = supertest(framework);


const AUTH_TOKEN = 'Bearer eyJ0eXAiOiJC1QiLCJhbGciOiJIS1NiJ9.eyJpc3MiOiJDWmwzVnRucWpWMmFIdGtXU29iMGJWU1lveGRGNFFCMCIsImlhdCI6MTYwODUxNzg1NCwiZXhwIjoxNjA3NTE4Mjc0LCJwcm9maWxlIjoiYWRtaW4iLCJnc21cHMiOlsxXSwidXNlcmlkIRCJqdGkiOiI1ZjY3YWZhN2Q2M2YyOWFlM2I3Y2VmYzUwODBlMmRlYiIsInNlcnZpY2UiOiJhZG1pbiIsInVzZXJuYW1lIjoicWRtaW4ifQ.hdljpZ7cIsRTI5x-43uWWzKprjWcFF9CCg1dCxLK9QA';

//jest.mock('axios');

const caseOne = {
  request: {
    attr: ['temperature'],
    lastN: 2,
  },
  expected:
    [{
      attr: 'temperature', device_id: 'a1b1c1', metadata: {}, ts: '2021-02-13T15:12:10.579Z', value: '10',
    }, {
      attr: 'temperature', device_id: 'a1b1c1', metadata: {}, ts: '2021-02-13T15:13:09.579Z', value: '9',
    }],
};


const caseTwo = {
  request: {
    attr: ['unknown_attribute'],
    lastN: 2,
  },
  expected:
    { "description": "No data for the given attribute could be found", "title": "Attr not found" },
};


const caseThree = {
  request: {
    attr: [],
    lastN: 2,
  },
  expected:
  {
    "temperature": [
      {
        "attr": "temperature",
        "device_id": "a1b1c1",
        "metadata": {},
        "ts": "2021-02-13T15:12:10.579Z",
        "value": "10",
      },
      {
        "attr": "temperature",
        "device_id": "a1b1c1",
        "metadata": {},
        "ts": "2021-02-13T15:13:09.579Z",
        "value": "9",
      },
    ],
  }
};



const caseFour = {
  request: {
    attr: ['temperature', 'atribute2'],
    lastN: 2,
  },
  expected:
  {
    "temperature": [
      {
        "attr": "temperature",
        "device_id": "a1b1c1",
        "metadata": {},
        "ts": "2021-02-13T15:12:10.579Z",
        "value": "10",
      },
      {
        "attr": "temperature",
        "device_id": "a1b1c1",
        "metadata": {},
        "ts": "2021-02-13T15:13:09.579Z",
        "value": "9",
      },
    ],
  }
};


describe('Unit testing to validate routes', () => {

  const fetchData = async (deviceId, currentCase) => {
    const paramList = currentCase.request;

    const pms = new URLSearchParams();
    pms.append("lastN", paramList.lastN);
    paramList.attr.forEach(attr => {
      pms.append("attr", attr);
    });

    return request
      .get(`/history/device/${deviceId}/history?${pms.toString()}`)
      .set('Authorization', AUTH_TOKEN)
      .set('Accept', 'application/json');
  };

  it('checking if header was been passed', async (done) => {
    const deviceId = 'a1b1c1';
    const response = await fetchData(deviceId, caseOne);
    expect(response.statusCode).toEqual(200);
    done();
  });

  it('checking response: 404 msg not found data ', async (done) => {
    const deviceId = 'a1b1c1';
    const response = await fetchData(deviceId, caseTwo);
    expect(response.statusCode).toEqual(404);
    expect(response.body).toEqual(caseTwo.expected);

    done();
  });



  it('checking if the correct data manipulation', async (done) => {
    const deviceId = 'a1b1c1';
    const response = await fetchData(deviceId, caseOne);
    expect(response.body).toEqual(caseOne.expected);
    done();
  });



  it('requesting all atributes', async (done) => {
    const deviceId = 'a1b1c1';
    const response = await fetchData(deviceId, caseThree);
    expect(response.body).toEqual(caseThree.expected);
    done();
  });


  it('sending multiple atributes', async (done) => {
    const deviceId = 'a1b1c1';
    const response = await fetchData(deviceId, caseFour);
    expect(response.body).toEqual(caseFour.expected);
    done();
  });

  afterAll(async () => {
    await new Promise((resolve) => setTimeout(() => resolve(), 500));
  });
});
