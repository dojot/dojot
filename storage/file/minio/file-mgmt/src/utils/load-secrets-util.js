/* eslint-disable no-param-reassign */
const fs = require('fs');

async function loadSecrets(config) {
  if (config.minio['access.key.file']) {
    // It is necessary to read a file in an absolute path   
    // eslint-disable-next-line security/detect-non-literal-fs-filename
    const accessKey = await fs.promises.readFile(`/secrets/${config.minio['access.key.file']}`, 'utf-8');
    config.minio['access.key'] = accessKey.replace('\n', '');
  }
  
  if (config.minio['secret.key.file']) {
    // It is necessary to read a file in an absolute path
    // eslint-disable-next-line security/detect-non-literal-fs-filename
    const secretKey = await fs.promises.readFile(`/secrets/${config.minio['secret.key.file']}`, 'utf-8');
    config.minio['secret.key'] = secretKey.replace('\n', '');
  }
}

module.exports = {
  loadSecrets,
};
