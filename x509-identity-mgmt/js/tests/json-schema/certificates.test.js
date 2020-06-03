const request = require('supertest');

const faker = require('faker');

const {
  generateCert, generateCSR, p256CSR, p256Cert, token,
} = require('../util.test');

const app = require('../../src/app');

const req = request(app);

describe('X509 Certificates - JSON Schema validations [on http POST]', () => {
  // --------------------
  // test required fields
  // --------------------
  it("should have required property 'certificatePem' or 'csr'",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send()
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'required',
              dataPath: '',
              schemaPath: '#/oneOf/0/required',
              params: {
                missingProperty: 'certificatePem',
              },
              message: "should have required property 'certificatePem'",
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
        });
      }));

  it("should match exactly one schema in oneOf ('certificatePem' or 'csr')",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        certificatePem: p256Cert,
        csr: p256CSR,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'not',
              dataPath: '',
              schemaPath: '#/dependencies/certificatePem/not',
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
        });
      }));

  it("should match exactly one schema in oneOf ('belongsTo.device' or 'belongsTo.application')",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        certificatePem: p256Cert,
        belongsTo: {
          device: '00000000-0000-0000-0000-000000000000',
          application: 'kafka-consumer',
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'not',
              dataPath: '.belongsTo',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/dependencies/device/not',
              params: {},
              message: 'should NOT be valid',
            },
            {
              keyword: 'not',
              dataPath: '.belongsTo',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/dependencies/application/not',
              params: {},
              message: 'should NOT be valid',
            },
            {
              keyword: 'oneOf',
              dataPath: '.belongsTo',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/oneOf',
              params: {
                passingSchemas: [
                  0,
                  1,
                ],
              },
              message: 'should match exactly one schema in oneOf',
            },
          ],
        });
      }));

  // --------------------
  // test all field types
  // --------------------
  it("should 'certificatePem' be string",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        certificatePem: null,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'type',
              dataPath: '.certificatePem',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/cert-pem/type',
              params: {
                type: 'string',
              },
              message: 'should be string',
            },
          ],
        });
      }));

  it("should 'csr' be string",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        csr: null,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'type',
              dataPath: '.csr',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/csr/type',
              params: {
                type: 'string',
              },
              message: 'should be string',
            },
          ],
        });
      }));

  it("should 'belongsTo.device' be string",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        certificatePem: p256Cert,
        belongsTo: {
          device: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'type',
              dataPath: '.belongsTo.device',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/device/type',
              params: {
                type: 'string,null',
              },
              message: 'should be string,null',
            },
          ],
        });
      }));

  it("should 'belongsTo.application' be an enumerated string",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        certificatePem: p256Cert,
        belongsTo: {
          application: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'type',
              dataPath: '.belongsTo.application',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/application/type',
              params: {
                type: 'string,null',
              },
              message: 'should be string,null',
            },
            {
              keyword: 'enum',
              dataPath: '.belongsTo.application',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/application/enum',
              params: {
                allowedValues: [
                  'kafka-consumer',
                ],
              },
              message: 'should be equal to one of the allowed values',
            },
          ],
        });
      }));


  // ---------------------
  // test all field limits
  // ---------------------
  it("should 'certificatePem' NOT be longer than 65536 characters",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        certificatePem: generateCert(65536 + 1), /* (+1 to test) */
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [{
            dataPath: '.certificatePem',
            keyword: 'maxLength',
            message: 'should NOT be longer than 65536 characters',
            params: {
              limit: 65536,
            },
            schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/cert-pem/maxLength',
          },
          ],
        });
      }));

  it("should 'csr' NOT be longer than 65536 characters",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        csr: generateCSR(65536 + 1), /* (+1 to test) */
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              dataPath: '.csr',
              keyword: 'maxLength',
              message: 'should NOT be longer than 65536 characters',
              params: {
                limit: 65536,
              },
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/csr/maxLength',
            },
          ],
        });
      }));

  // --------------------
  // test all field regex
  // --------------------
  it("should 'certificatePem' match the regex pattern",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        certificatePem: '',
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'pattern',
              dataPath: '.certificatePem',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/cert-pem/pattern',
              params: {
                pattern: '^(-{5}BEGIN CERTIFICATE-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE-{5})+$',
              },
              message: 'should match pattern "^(-{5}BEGIN CERTIFICATE-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE-{5})+$"',
            },
          ],
        });
      }));

  it("should 'csr' match the regex pattern",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        csr: '',
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'pattern',
              dataPath: '.csr',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/csr/pattern',
              params: {
                pattern: '^-{5}BEGIN CERTIFICATE REQUEST-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE REQUEST-{5}$',
              },
              message: 'should match pattern "^-{5}BEGIN CERTIFICATE REQUEST-{5}(\\r\\n|\\r|\\n)([-A-Za-z0-9+/=]{1,64}(\\r\\n|\\r|\\n))+-{5}END CERTIFICATE REQUEST-{5}$"',
            },
          ],
        });
      }));

  it("should 'belongsTo.device' match the regex pattern",
    () => req.post('/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        certificatePem: p256Cert,
        belongsTo: {
          device: '',
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/reg-or-gen-cert',
          schemaErrors: [
            {
              keyword: 'pattern',
              dataPath: '.belongsTo.device',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/device/pattern',
              params: {
                pattern: '^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$',
              },
              message: 'should match pattern "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"',
            },
          ],
        });
      }));
});

describe('X509 Certificates - JSON Schema validations [on http PATCH]', () => {
  // --------------------
  // test required fields
  // --------------------
  it("should have required property 'belongsTo'",
    () => req.patch(`/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .set('Authorization', `Bearer ${token}`)
      .send()
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/ch-owner-cert',
          schemaErrors: [
            {
              keyword: 'required',
              dataPath: '',
              schemaPath: '#/required',
              params: {
                missingProperty: 'belongsTo',
              },
              message: "should have required property 'belongsTo'",
            },
          ],
        });
      }));

  it("should 'belongsTo' match exactly one schema in oneOf ('belongsTo.device' or 'belongsTo.application')",
    () => req.patch(`/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .set('Authorization', `Bearer ${token}`)
      .send({
        belongsTo: {
          device: 1010,
          application: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/ch-owner-cert',
          schemaErrors: [
            {
              keyword: 'not',
              dataPath: '.belongsTo',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/dependencies/device/not',
              params: {},
              message: 'should NOT be valid',
            },
            {
              keyword: 'not',
              dataPath: '.belongsTo',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/dependencies/application/not',
              params: {},
              message: 'should NOT be valid',
            },
            {
              keyword: 'type',
              dataPath: '.belongsTo.device',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/device/type',
              params: {
                type: 'string,null',
              },
              message: 'should be string,null',
            },
            {
              keyword: 'type',
              dataPath: '.belongsTo.application',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/application/type',
              params: {
                type: 'string,null',
              },
              message: 'should be string,null',
            },
            {
              keyword: 'enum',
              dataPath: '.belongsTo.application',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/application/enum',
              params: {
                allowedValues: [
                  'kafka-consumer',
                ],
              },
              message: 'should be equal to one of the allowed values',
            },
            {
              keyword: 'oneOf',
              dataPath: '.belongsTo',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/oneOf',
              params: {
                passingSchemas: [
                  0,
                  1,
                ],
              },
              message: 'should match exactly one schema in oneOf',
            },
          ],
        });
      }));


  // --------------------
  // test all field types
  // --------------------
  it("should 'belongsTo' be object",
    () => req.patch(`/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .set('Authorization', `Bearer ${token}`)
      .send({
        belongsTo: null,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/ch-owner-cert',
          schemaErrors: [
            {
              keyword: 'type',
              dataPath: '.belongsTo',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/type',
              params: {
                type: 'object',
              },
              message: 'should be object',
            },
            {
              keyword: 'oneOf',
              dataPath: '.belongsTo',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/oneOf',
              params: {
                passingSchemas: [
                  0,
                  1,
                ],
              },
              message: 'should match exactly one schema in oneOf',
            },
          ],
        });
      }));


  it("should 'belongsTo.device' should be string",
    () => req.patch(`/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .set('Authorization', `Bearer ${token}`)
      .send({
        belongsTo: {
          device: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/ch-owner-cert',
          schemaErrors: [
            {
              keyword: 'type',
              dataPath: '.belongsTo.device',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/device/type',
              params: {
                type: 'string,null',
              },
              message: 'should be string,null',
            },
          ],
        });
      }));

  it("should 'belongsTo.application' be an enumerated string",
    () => req.patch(`/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .set('Authorization', `Bearer ${token}`)
      .send({
        belongsTo: {
          application: 1010,
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/ch-owner-cert',
          schemaErrors: [
            {
              keyword: 'type',
              dataPath: '.belongsTo.application',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/application/type',
              params: {
                type: 'string,null',
              },
              message: 'should be string,null',
            },
            {
              keyword: 'enum',
              dataPath: '.belongsTo.application',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/application/enum',
              params: {
                allowedValues: [
                  'kafka-consumer',
                ],
              },
              message: 'should be equal to one of the allowed values',
            },
          ],
        });
      }));


  // --------------------
  // test all field regex
  // --------------------
  it("should 'belongsTo.device' match the regex pattern",
    () => req.patch(`/v1/certificates/${faker.random.alphaNumeric(32).toString(16)}`)
      .set('Authorization', `Bearer ${token}`)
      .send({
        belongsTo: {
          device: '',
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          schema$id: 'http://www.dojot.com.br/schemas/ch-owner-cert',
          schemaErrors: [
            {
              keyword: 'pattern',
              dataPath: '.belongsTo.device',
              schemaPath: 'http://www.dojot.com.br/schemas/defs#/definitions/belongsTo/properties/device/pattern',
              params: {
                pattern: '^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$',
              },
              message: 'should match pattern "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"',
            },
          ],
        });
      }));
});
