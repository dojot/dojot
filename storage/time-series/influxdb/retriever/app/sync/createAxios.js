const { default: axios } = require('axios');

module.exports = () => axios.create({
  timeout: 12000,
});
