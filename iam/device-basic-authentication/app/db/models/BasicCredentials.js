/* eslint-disable func-names */
const mongoose = require('mongoose');
const bcrypt = require('bcrypt');

const { Schema } = mongoose;

const BasicCredentialSchema = new Schema({
  deviceId: String,
  tenant: String,
  password: String,
});

const {
  ConfigManager: { getConfig },
} = require('@dojot/microservice-sdk');

const {
  credentials: credentialsConfig,
} = getConfig('BASIC_AUTH');

// Hash the user's password before saving it
BasicCredentialSchema.pre('findOneAndUpdate', function (next) {
  const { password } = this.getUpdate();
  if (!password) next();

  const salt = bcrypt.genSaltSync(credentialsConfig.hash.salt.rounds);
  const hash = bcrypt.hashSync(password, salt);
  this.getUpdate().password = hash;
  next();
});

// Compare the password hash with the one stored in the database
BasicCredentialSchema.methods.comparePassword = function (password) {
  return bcrypt.compareSync(password, this.password);
};

// Export the model
module.exports = mongoose.model('BasicCredential', BasicCredentialSchema);
