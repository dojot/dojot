const { Logger, WebUtils } = require('@dojot/microservice-sdk');

const request = require('supertest');

const faker = require('faker');

const { generateCert } = require('../util.test');

const framework = WebUtils.framework.createExpress({
  logger: new Logger('trustedCAs.test.js'),
  interceptors: [
    global.jsonBodyParsingInterceptor,
  ],
  routes: ([
    global.trustedCARoutes,
  ]).flat(),
  supportTrustProxy: global.config.framework.trustproxy,
});

const req = request(framework);

describe('Trusted CAs - JSON Schema validations [on http POST]', () => {
  // --------------------
  // test required fields
  // --------------------
  it("should have required property 'caPem'",
    () => req.post('/api/v1/trusted-cas')
      .send()
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'http://www.dojot.com.br/schemas/reg-trust-ca',
            schemaErrors: [
              {
                keyword: 'required',
                dataPath: '',
                schemaPath: '#/required',
                params: {
                  missingProperty: 'caPem',
                },
                message: "should have required property 'caPem'",
              },
            ],
          },
        });
      }));

  // --------------------
  // test all field types
  // --------------------
  it("should 'caPem' be string and 'allowAutoRegistration' be boolean",
    () => req.post('/api/v1/trusted-cas')
      .send({
        caPem: null,
        allowAutoRegistration: null,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'http://www.dojot.com.br/schemas/reg-trust-ca',
            schemaErrors: [
              {
                keyword: 'type',
                dataPath: '.caPem',
                schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/cert-pem/type',
                params: {
                  type: 'string',
                },
                message: 'should be string',
              },
              {
                keyword: 'type',
                dataPath: '.allowAutoRegistration',
                schemaPath: '#/properties/allowAutoRegistration/type',
                params: {
                  type: 'boolean',
                },
                message: 'should be boolean',
              },
            ],
          },
        });
      }));

  // ---------------------
  // test all field limits
  // ---------------------
  it("should 'caPem' NOT be longer than 65536 characters",
    () => req.post('/api/v1/trusted-cas')
      .send({
        caPem: generateCert(65536 + 1), /* (+1 to test) */
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'http://www.dojot.com.br/schemas/reg-trust-ca',
            schemaErrors: [
              {
                dataPath: '.caPem',
                keyword: 'maxLength',
                message: 'should NOT be longer than 65536 characters',
                params: {
                  limit: 65536,
                },
                schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/cert-pem/maxLength',

              },
            ],
          },
        });
      }));

  // --------------------
  // test all field regex
  // --------------------
  it("should 'caPem' should match the regex pattern",
    () => req.post('/api/v1/trusted-cas')
      .send({
        caPem: '',
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'http://www.dojot.com.br/schemas/reg-trust-ca',
            schemaErrors: [
              {
                keyword: 'pattern',
                dataPath: '.caPem',
                schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/cert-pem/pattern',
                params: {
                  pattern: '^(-{5}BEGIN CERTIFICATE-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE-{5}(\\r\\n|\\r|\\n)?)+$',
                },
                message: 'should match pattern "^(-{5}BEGIN CERTIFICATE-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE-{5}(\\r\\n|\\r|\\n)?)+$"',
              },
            ],
          },
        });
      }));
});

describe('Trusted CAs - JSON Schema validations [on http PATCH]', () => {
  // --------------------
  // test required fields
  // --------------------
  it("should have required property 'allowAutoRegistration'",
    () => req.patch(`/api/v1/trusted-cas/${faker.random.alphaNumeric(32).toString(16)}`)
      .send()
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'http://www.dojot.com.br/schemas/upd-trust-ca',
            schemaErrors: [
              {
                keyword: 'required',
                dataPath: '',
                schemaPath: '#/required',
                params: {
                  missingProperty: 'allowAutoRegistration',
                },
                message: "should have required property 'allowAutoRegistration'",
              },
            ],
          },
        });
      }));

  // --------------------
  // test all field types
  // --------------------
  it("should 'allowAutoRegistration' be boolean",
    () => req.patch(`/api/v1/trusted-cas/${faker.random.alphaNumeric(32).toString(16)}`)
      .send({
        allowAutoRegistration: null,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'http://www.dojot.com.br/schemas/upd-trust-ca',
            schemaErrors: [
              {
                keyword: 'type',
                dataPath: '.allowAutoRegistration',
                schemaPath: '#/properties/allowAutoRegistration/type',
                params: {
                  type: 'boolean',
                },
                message: 'should be boolean',
              },
            ],
          },
        });
      }));
});
