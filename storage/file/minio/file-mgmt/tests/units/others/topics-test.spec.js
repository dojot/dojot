const controllers = {
  kafkaController: {
    handleTenancy: jest.fn(),
  },
};

const config = {
  subscribe: {
    'topics.regex.tenants': '',
  },
};

const topics = require('../../../src/app/kafka/topics');

describe('/kafka/topics', () => {
  it('Should set routes', async () => {
    const routes = topics(config, controllers);
    expect(routes.length).toEqual(1);
  });
});
