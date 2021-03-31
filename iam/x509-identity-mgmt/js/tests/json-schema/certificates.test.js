const { Logger, WebUtils } = require('@dojot/microservice-sdk');

const request = require('supertest');

const faker = require('faker');

const {
  generateCert, generateCSR, p256CSR, certChain,
} = require('../util.test');

const certificateChain = certChain.join('\n').replace(/^(\s*)(.*)(\s*$)/gm, '$2');

const framework = WebUtils.framework.createExpress({
  logger: new Logger('certificates.test.js'),
  interceptors: [
    global.jsonBodyParsingInterceptor,
  ],
  routes: ([
    global.certificateRoutes,
  ]).flat(),
  supportTrustProxy: global.config.framework.trustproxy,
});

const req = request(framework);

describe('X509 Certificates - JSON Schema validations [on http POST]', () => {
  // --------------------
  // test required fields
  // --------------------
  it("should have required property 'certificateChain' or 'csr'",
    () => req.post('/api/v1/certificates')
      .send()
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [
              {
                keyword: 'required',
                dataPath: '',
                schemaPath: '#/oneOf/0/required',
                params: {
                  missingProperty: 'certificateChain',
                },
                message: "should have required property 'certificateChain'",
              },
              {
                keyword: 'required',
                dataPath: '',
                schemaPath: '#/oneOf/1/required',
                params: {
                  missingProperty: 'csr',
                },
                message: "should have required property 'csr'",
              },
              {
                keyword: 'oneOf',
                dataPath: '',
                schemaPath: '#/oneOf',
                params: {
                  passingSchemas: null,
                },
                message: 'should match exactly one schema in oneOf',
              },
            ],
          },
        });
      }));

  it("should match exactly one schema in oneOf ('certificateChain' or 'csr')",
    () => req.post('/api/v1/certificates')
      .send({
        certificateChain,
        csr: p256CSR,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [
              {
                keyword: 'not',
                dataPath: '',
                schemaPath: '#/dependencies/certificateChain/not',
                params: {},
                message: 'should NOT be valid',
              },
              {
                keyword: 'not',
                dataPath: '',
                schemaPath: '#/dependencies/csr/not',
                params: {},
                message: 'should NOT be valid',
              },
              {
                keyword: 'oneOf',
                dataPath: '',
                schemaPath: '#/oneOf',
                params: {
                  passingSchemas: [
                    0,
                    1,
                  ],
                },
                message: 'should match exactly one schema in oneOf',
              },
            ],
          },
        });
      }));

  it("should match exactly one schema in oneOf ('belongsTo.device' or 'belongsTo.application')",
    () => req.post('/api/v1/certificates')
      .send({
        certificateChain,
        belongsTo: {
          device: '1234567890',
          application: 'kafka-consumer',
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [
              {
                keyword: 'not',
                dataPath: '.belongsTo',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/dependencies/device/not',
                params: {},
                message: 'should NOT be valid',
              },
              {
                keyword: 'not',
                dataPath: '.belongsTo',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/dependencies/application/not',
                params: {},
                message: 'should NOT be valid',
              },
              {
                keyword: 'oneOf',
                dataPath: '.belongsTo',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/oneOf',
                params: {
                  passingSchemas: [
                    0,
                    1,
                  ],
                },
                message: 'should match exactly one schema in oneOf',
              },
            ],
          },
        });
      }));

  // --------------------
  // test all field types
  // --------------------
  it("should 'certificateChain' be string",
    () => req.post('/api/v1/certificates')
      .send({
        certificateChain: null,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [
              {
                keyword: 'type',
                dataPath: '.certificateChain',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/cert-pem/type',
                params: {
                  type: 'string',
                },
                message: 'should be string',
              },
            ],
          },
        });
      }));

  it("should 'csr' be string",
    () => req.post('/api/v1/certificates')
      .send({
        csr: null,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [
              {
                keyword: 'type',
                dataPath: '.csr',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/csr/type',
                params: {
                  type: 'string',
                },
                message: 'should be string',
              },
            ],
          },
        });
      }));

  it("should 'belongsTo.device' be string",
    () => req.post('/api/v1/certificates')
      .send({
        certificateChain,
        belongsTo: {
          device: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [
              {
                keyword: 'type',
                dataPath: '.belongsTo.device',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/properties/device/type',
                params: {
                  type: 'string,null',
                },
                message: 'should be string,null',
              },
            ],
          },
        });
      }));

  it("should 'belongsTo.application' be string",
    () => req.post('/api/v1/certificates')
      .send({
        certificateChain,
        belongsTo: {
          application: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [
              {
                keyword: 'type',
                dataPath: '.belongsTo.application',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/properties/application/type',
                params: {
                  type: 'string,null',
                },
                message: 'should be string,null',
              },
            ],
          },
        });
      }));

  // ---------------------
  // test all field limits
  // ---------------------
  it("should 'certificateChain' NOT be longer than 65536 characters",
    () => req.post('/api/v1/certificates')
      .send({
        certificateChain: generateCert(65536 + 1), /* (+1 to test) */
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [{
              dataPath: '.certificateChain',
              keyword: 'maxLength',
              message: 'should NOT be longer than 65536 characters',
              params: {
                limit: 65536,
              },
              schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/cert-pem/maxLength',
            }],
          },
        });
      }));

  it("should 'csr' NOT be longer than 65536 characters",
    () => req.post('/api/v1/certificates')
      .send({
        csr: generateCSR(65536 + 1), /* (+1 to test) */
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [{
              dataPath: '.csr',
              keyword: 'maxLength',
              message: 'should NOT be longer than 65536 characters',
              params: {
                limit: 65536,
              },
              schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/csr/maxLength',
            }],
          },
        });
      }));

  // --------------------
  // test all field regex
  // --------------------
  it("should 'certificateChain' match the regex pattern",
    () => req.post('/api/v1/certificates')
      .send({
        certificateChain: '',
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [{
              keyword: 'pattern',
              dataPath: '.certificateChain',
              schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/cert-pem/pattern',
              params: {
                pattern: '^(-{5}BEGIN CERTIFICATE-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE-{5}(\\r\\n|\\r|\\n)?)+$',
              },
              message: 'should match pattern "^(-{5}BEGIN CERTIFICATE-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE-{5}(\\r\\n|\\r|\\n)?)+$"',
            }],
          },
        });
      }));

  it("should 'csr' match the regex pattern",
    () => req.post('/api/v1/certificates')
      .send({
        csr: '',
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [{
              keyword: 'pattern',
              dataPath: '.csr',
              schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/csr/pattern',
              params: {
                pattern: '^-{5}BEGIN CERTIFICATE REQUEST-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE REQUEST-{5}(\\r\\n|\\r|\\n)?$',
              },
              message: 'should match pattern "^-{5}BEGIN CERTIFICATE REQUEST-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE REQUEST-{5}(\\r\\n|\\r|\\n)?$"',
            }],
          },
        });
      }));

  it("should 'belongsTo.device' match the regex pattern",
    () => req.post('/api/v1/certificates')
      .send({
        certificateChain,
        belongsTo: {
          device: '',
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/register-or-generate-certificate.json',
            schemaErrors: [{
              keyword: 'pattern',
              dataPath: '.belongsTo.device',
              schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/properties/device/pattern',
              params: {
                pattern: '^[0-9a-fA-F-]{6,36}$',
              },
              message: 'should match pattern "^[0-9a-fA-F-]{6,36}$"',
            }],
          },
        });
      }));
});

describe('X509 Certificates - JSON Schema validations [on http PATCH]', () => {
  // --------------------
  // test required fields
  // --------------------
  it("should have required property 'belongsTo'",
    () => req.patch(`/api/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .send()
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/change-owner-certificate.json',
            schemaErrors: [{
              keyword: 'required',
              dataPath: '',
              schemaPath: '#/required',
              params: {
                missingProperty: 'belongsTo',
              },
              message: "should have required property 'belongsTo'",
            }],
          },
        });
      }));

  it("should 'belongsTo' match exactly one schema in oneOf ('belongsTo.device' or 'belongsTo.application')",
    () => req.patch(`/api/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .send({
        belongsTo: {
          device: 1010,
          application: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/change-owner-certificate.json',
            schemaErrors: [
              {
                keyword: 'not',
                dataPath: '.belongsTo',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/dependencies/device/not',
                params: {},
                message: 'should NOT be valid',
              },
              {
                keyword: 'not',
                dataPath: '.belongsTo',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/dependencies/application/not',
                params: {},
                message: 'should NOT be valid',
              },
              {
                keyword: 'type',
                dataPath: '.belongsTo.device',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/properties/device/type',
                params: {
                  type: 'string,null',
                },
                message: 'should be string,null',
              },
              {
                keyword: 'type',
                dataPath: '.belongsTo.application',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/properties/application/type',
                params: {
                  type: 'string,null',
                },
                message: 'should be string,null',
              },
              {
                keyword: 'oneOf',
                dataPath: '.belongsTo',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/oneOf',
                params: {
                  passingSchemas: [
                    0,
                    1,
                  ],
                },
                message: 'should match exactly one schema in oneOf',
              },
            ],
          },
        });
      }));

  // --------------------
  // test all field types
  // --------------------
  it("should 'belongsTo' be object",
    () => req.patch(`/api/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .send({
        belongsTo: null,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/change-owner-certificate.json',
            schemaErrors: [
              {
                keyword: 'type',
                dataPath: '.belongsTo',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/type',
                params: {
                  type: 'object',
                },
                message: 'should be object',
              },
              {
                keyword: 'oneOf',
                dataPath: '.belongsTo',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/oneOf',
                params: {
                  passingSchemas: [
                    0,
                    1,
                  ],
                },
                message: 'should match exactly one schema in oneOf',
              },
            ],
          },
        });
      }));

  it("should 'belongsTo.device' should be string",
    () => req.patch(`/api/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .send({
        belongsTo: {
          device: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/change-owner-certificate.json',
            schemaErrors: [{
              keyword: 'type',
              dataPath: '.belongsTo.device',
              schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/properties/device/type',
              params: {
                type: 'string,null',
              },
              message: 'should be string,null',
            }],
          },
        });
      }));

  it("should 'belongsTo.application' be an enumerated string",
    () => req.patch(`/api/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .send({
        belongsTo: {
          application: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/change-owner-certificate.json',
            schemaErrors: [
              {
                keyword: 'type',
                dataPath: '.belongsTo.application',
                schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/properties/application/type',
                params: {
                  type: 'string,null',
                },
                message: 'should be string,null',
              },
            ],
          },
        });
      }));

  // --------------------
  // test all field regex
  // --------------------
  it("should 'belongsTo.device' match the regex pattern",
    () => req.patch(`/api/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .send({
        belongsTo: {
          device: '',
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/change-owner-certificate.json',
            schemaErrors: [{
              keyword: 'pattern',
              dataPath: '.belongsTo.device',
              schemaPath: 'https://raw.githubusercontent.com/dojot/dojot/development/iam/x509-identity-mgmt/js/schemas/defs.json#/definitions/belongsTo/properties/device/pattern',
              params: {
                pattern: '^[0-9a-fA-F-]{6,36}$',
              },
              message: 'should match pattern "^[0-9a-fA-F-]{6,36}$"',
            }],
          },
        });
      }));
});
