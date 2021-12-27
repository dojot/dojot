const { generateApp, sendFakePayload } = require('./setup');

describe('TOPIC *.dojot.tenancy', () => {
  let app;
  beforeAll(async () => {
    app = generateApp();
    await app.init();
  });

  it('Should create tenant when the message type is CREATE', (done) => {
    const message = '{ "type": "CREATE", "tenant": "test" }';

    sendFakePayload(
      'dojot.tenancy', message, (error) => {
        expect(error).toBeUndefined();
        done();
      },
    );
  });

  it('Should remove tenant when the message type is DELETE ', (done) => {
    const message = '{ "type": "DELETE", "tenant": "test" }';

    sendFakePayload(
      'dojot.tenancy', message, (error) => {
        expect(error).toBeUndefined();
        done();
      },
    );
  });

  it('Should return an error ', (done) => {
    const message = '{ "type": "ERROR", "tenant": "test" }';

    sendFakePayload(
      'dojot.tenancy', message, (error) => {
        expect(error).toBeDefined();
        done();
      },
    );
  });
});
