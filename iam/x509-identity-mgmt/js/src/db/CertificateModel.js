const mongoose = require('mongoose');

const MongoQS = require('mongo-querystring');

const CommonModel = require('./CommonModel');

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

const mongooseModel = mongoose.model('Certificate', certificateSchema);

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

const mongoQS = Object.freeze(new MongoQS({
  keyRegex: /^[a-zA-Z0-9-_.]+$/i,
  arrRegex: /^[a-zA-Z0-9-_.]+(\[])?$/i,
  whitelist: Object.freeze({
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
  }),
}));

class CertificateModel extends CommonModel {
  constructor({ mongoClient }) {
    super({
      mongoClient, mongooseModel, mongoQS, projectableFields,
    });
  }
}

module.exports = CertificateModel;
