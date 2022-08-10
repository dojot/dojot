const mongoose = require('mongoose');

const MongoQS = require('mongo-querystring');

const CommonModel = require('./CommonModel');

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

const sortByFields = [
  'caFingerprint',
  'asc:caFingerprint',
  'desc:caFingerprint',
  'subjectDN',
  'asc:subjectDN',
  'desc:subjectDN',
  'validity.notBefore',
  'asc:validity.notBefore',
  'desc:validity.notBefore',
  'validity.notAfter',
  'asc:validity.notAfter',
  'desc:validity.notAfter',
  'createdAt',
  'asc:createdAt',
  'desc:createdAt',
  'modifiedAt',
  'asc:modifiedAt',
  'desc:modifiedAt',
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

class TrustedCAModel extends CommonModel {
  constructor({ mongoClient }) {
    super({
      mongoClient, mongooseModel, mongoQS, projectableFields, sortByFields,
    });
  }
}

module.exports = TrustedCAModel;
