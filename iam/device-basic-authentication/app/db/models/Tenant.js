const mongoose = require('mongoose');

const { Schema } = mongoose;

const TenantSchema = new Schema({
  tenant: String,
});

// Export the model
module.exports = mongoose.model('Tenant', TenantSchema);
