const jwt = require('jsonwebtoken');
const { promisify } = require('util');

const jwtSignAsync = promisify(jwt.sign).bind(jwt);

function createTokenGen() {
  const generate = async (tenant) => {
    const expirationSec = 60;
    const token = await jwtSignAsync({
      service: tenant,
    }, 'secret', { expiresIn: expirationSec });
    return token;
  };
  return {
    generate,
  };
}

module.exports = () => createTokenGen();
