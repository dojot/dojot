/* eslint-disable no-param-reassign */
const fs = require('fs');

async function loadSecrets(config) {
  if (config.minio['access.key.file']) {
    const accessKey = await fs.promises.readFile(`/secrets/${config.minio['access.key.file']}`, 'utf-8');
    config.minio['access.key'] = accessKey.replace('\n', '');
  }
  
  if (config.minio['secret.key.file']) {
    const secretKey = await fs.promises.readFile(`/secrets/${config.minio['secret.key.file']}`, 'utf-8');
    config.minio['secret.key'] = secretKey.replace('\n', '');
  }
}

module.exports = {
  loadSecrets,
};
