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
  parseConditionFields: (urlQueryStringObj) => mongoQS.parse(urlQueryStringObj),
  parseProjectionFields: (commaSeparatedFields) => (
    db.parseProjectionFields(commaSeparatedFields, projectableFields)
  ),
  sanitizeFields: (cert) => db.sanitizeFields(cert, projectableFields),
});
