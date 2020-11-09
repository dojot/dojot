const mongoose = require('mongoose');

const MongoQS = require('mongo-querystring');

const { Schema } = mongoose;

const trustedCASchema = new Schema({
  caFingerprint: String,
  caPem: String,
  subjectDN: String,
  validity: new Schema({
    notBefore: Date,
    notAfter: Date,
  }),
  createdAt: { type: Date, default: Date.now },
  modifiedAt: { type: Date, default: Date.now },
  allowAutoRegistration: { type: Boolean, default: false },
  tenant: String,
});
trustedCASchema.index({ tenant: 1, caFingerprint: 1 });

const mongooseModel = mongoose.model('TrustedCA', trustedCASchema);

const projectableFields = [
  'caFingerprint',
  'caPem',
  'subjectDN',
  'validity',
  'validity.notBefore',
  'validity.notAfter',
  'createdAt',
  'modifiedAt',
  'allowAutoRegistration',
  'tenant',
];

const mongoQS = Object.freeze(new MongoQS({
  keyRegex: /^[a-zA-Z0-9-_.]+$/i,
  arrRegex: /^[a-zA-Z0-9-_.]+(\[])?$/i,
  whitelist: Object.freeze({
    caFingerprint: true,
    caPem: true,
    subjectDN: true,
    'validity.notBefore': true,
    'validity.notAfter': true,
    createdAt: true,
    modifiedAt: true,
    allowAutoRegistration: true,
    tenant: true,
  }),
}));

class TrustedCAModel {
  constructor({ db }) {
    Object.defineProperty(this, 'db', { value: db });
    Object.defineProperty(this, 'model', { value: mongooseModel });
    Object.defineProperty(this, 'mongoQS', { value: mongoQS });
  }

  parseConditionFields(candidates) {
    const conditionFields = { ...candidates };
    Object.entries(conditionFields).forEach(
      ([key, value]) => {
        // value must be a string
        if (typeof value !== 'string') {
          if (typeof value.toString !== 'function') {
            throw new Error('The value of the Condition Field must be a string or convertible to a string.');
          }
          Reflect.set(conditionFields, key, value.toString());
        }
      },
    );
    return this.mongoQS.parse(conditionFields);
  }

  parseProjectionFields(commaSeparatedFields) {
    return this.db.parseProjectionFields(commaSeparatedFields, projectableFields);
  }

  sanitizeFields(cert) {
    return this.db.sanitizeFields(cert, projectableFields);
  }
}

module.exports = TrustedCAModel;
