const mongoose = require('mongoose');

const MongoQS = require('mongo-querystring');

const { Schema } = mongoose;

const certificateSchema = new Schema({
  caFingerprint: String,
  fingerprint: String,
  pem: String,
  subjectDN: String,
  validity: new Schema({
    notBefore: Date,
    notAfter: Date,
  }),
  createdAt: { type: Date, default: Date.now },
  modifiedAt: { type: Date, default: Date.now },
  issuedByDojotPki: { type: Boolean, default: true },
  autoRegistered: { type: Boolean, default: false },
  belongsTo: new Schema({
    device: String,
    application: String,
  }),
  tenant: String,
});
certificateSchema.index({ tenant: 1, fingerprint: 1 });

const projectableFields = [
  'fingerprint',
  'caFingerprint',
  'pem',
  'subjectDN',
  'validity',
  'validity.notBefore',
  'validity.notAfter',
  'createdAt',
  'modifiedAt',
  'issuedByDojotPki',
  'autoRegistered',
  'belongsTo',
  'belongsTo.device',
  'belongsTo.application',
  'tenant',
];

const mongoQS = new MongoQS({
  keyRegex: /^[a-zA-Z0-9-_.]+$/i,
  arrRegex: /^[a-zA-Z0-9-_.]+(\[])?$/i,
  whitelist: {
    fingerprint: true,
    caFingerprint: true,
    pem: true,
    subjectDN: true,
    'validity.notBefore': true,
    'validity.notAfter': true,
    createdAt: true,
    modifiedAt: true,
    issuedByDojotPki: true,
    autoRegistered: true,
    'belongsTo.device': true,
    'belongsTo.application': true,
    tenant: true,
  },
});

module.exports = ({ db }) => ({
  model: mongoose.model('Certificate', certificateSchema),
  parseConditionFields: (conditionFields) => {
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
    return mongoQS.parse(conditionFields);
  },
  parseProjectionFields: (commaSeparatedFields) => (
    db.parseProjectionFields(commaSeparatedFields, projectableFields)
  ),
  sanitizeFields: (cert) => db.sanitizeFields(cert, projectableFields),
});
