
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

const mongoQS = new MongoQS({
  keyRegex: /^[a-zA-Z0-9-_.]+$/i,
  arrRegex: /^[a-zA-Z0-9-_.]+(\[])?$/i,
  whitelist: {
    caFingerprint: true,
    caPem: true,
    subjectDN: true,
    'validity.notBefore': true,
    'validity.notAfter': true,
    createdAt: true,
    modifiedAt: true,
    allowAutoRegistration: true,
    tenant: true,
  },
});

module.exports = ({ db }) => ({
  model: mongoose.model('TrustedCA', trustedCASchema),
  parseConditionFields: (urlQueryStringObj) => mongoQS.parse(urlQueryStringObj),
  parseProjectionFields: (commaSeparatedFields) => (
    db.parseProjectionFields(commaSeparatedFields, projectableFields)
  ),
  sanitizeFields: (cert) => db.sanitizeFields(cert, projectableFields),
});
