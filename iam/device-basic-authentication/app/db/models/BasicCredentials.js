/* eslint-disable func-names */
const mongoose = require('mongoose');
const bcrypt = require('bcrypt');

const { Schema } = mongoose;

const BasicCredentialSchema = new Schema({
  deviceId: String,
  tenant: String,
  password: String,
});

// Hash the user's password before saving it
BasicCredentialSchema.pre('save', function (next) {
  const credential = this;
  if (this.isModified('password') || this.isNew) {
    const salt = bcrypt.genSaltSync(10);
    const hash = bcrypt.hashSync(credential.password, salt);
    credential.password = hash;
    return next();
  }
  next();
});

// Compare the password hash with the one stored in the database
BasicCredentialSchema.methods.comparePassword = function (password) {
  return bcrypt.compareSync(password, this.password);
};

// Export the model
module.exports = mongoose.model('BasicCredential', BasicCredentialSchema);
