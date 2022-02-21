const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger('device-manager-sidecar:utils');

var jwt = require('jsonwebtoken');

const generateJwt = (tenant) => {
    logger.debug(`GenerateJwt: generating jwt for ${tenant}`);
    let encode_data = {
        'userid': 1, 'name': 'Admin (superuser)', 'groups': [1], 'iat':
            1517339633, 'exp': 1517340053, 'email': 'admin@noemail.com', 'profile':
            'admin', 'iss': 'eGfIBvOLxz5aQxA92lFk5OExZmBMZDDh', 'service': tenant,
        'jti': '7e3086317df2c299cef280932da856e5', 'username': 'admin'
    }
    try {
        return jwt.sign(encode_data, 'secret');
    } catch (e) {
      logger.error('GenerateJwt:', e);
      throw e;
    }
  };

module.exports={generateJwt}


