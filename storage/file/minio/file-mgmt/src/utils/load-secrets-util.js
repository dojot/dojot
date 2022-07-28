const fs = require('fs');
/*
* This class is intended to handle secret entry through files
* in dojot microservices.
*/
module.exports = class SecretFileHandler {
  /**
   * @constructor
   *
   * @param {Object} config Environment variables that were setting
   * @param {Logger} logger Dojot logger
   */
  constructor(config, logger) {
    this.config = config;
    this.logger = logger;
  }

  /**
   * This method watches a collection of secret files and gets
   * them as soon as possible.
   *
   * @param {string} keyPaths The keys path of the environment variables that must be observed.
   * @param {string} dirPath Directory path where the secrets should be.
   *
   * Note: The secrets will be updated in the environment variable settings
   * that were entered in SecretFileHandler
   *
   * @public
   */
  handleCollection(keyPaths, dirPath = '/secret/') {
    const outerThis = this;
    return new Promise((resolve, reject) => {
      const keyPathsResolved = [];
      this.logger.info('Loading secrets...');
      keyPaths.forEach((keyPath) => {
        outerThis.handle(keyPath, dirPath).then(() => {
          keyPathsResolved.push(keyPath);
          if (keyPaths.length === keyPathsResolved.length) {
            this.logger.info('Loaded secrets...');
            resolve();
          }
        }).catch((error) => {
          reject(error);
        });
      });
    });
  }


  /**
   * This method watches one secret file and gets
   * it as soon as possible.
   *
   * @param {string} keyPath The key path of the environment variable that must be observed.
   * @param {string} dirPath Directory path where the secret should be.
   *
   * Note: The secret will be updated in the environment variable settings
   * that were entered in SecretFileHandler
   *
   * @public
   */
  handle(keyPath, dirPath = '/secret/') {
    const outerThis = this;
    return new Promise((resolve, reject) => {
      const success = () => {
        outerThis.logger.debug(`Loaded secret to ${keyPath}.`);
        resolve();
      };

      const fails = (error) => {
        outerThis.logger.error(error);
        reject(error);
      };

      const secretfile = outerThis.getEnv(`${keyPath}.file`);

      if (secretfile) {
        outerThis.logger.debug(`Loading secret to ${keyPath} from a file.`);

        // eslint-disable-next-line security/detect-non-literal-fs-filename
        fs.promises.readFile(`${dirPath}${secretfile}`, 'utf-8').then((raw) => {
          outerThis.logger.debug('Secret file found');

          outerThis.setEnv(keyPath, raw.replace('\n', ''));
          success();
        }).catch(() => {
          outerThis.logger.debug('Waiting for secret file');
          try {
            // eslint-disable-next-line security/detect-non-literal-fs-filename
            const watcher = fs.watch(`${dirPath}`, { interval: 60 }, async (type, filename) => {
              try {
                if (type === 'rename' && filename === secretfile) {
                  outerThis.logger.debug('Secret file found');

                  // eslint-disable-next-line security/detect-non-literal-fs-filename
                  const raw = await fs.promises.readFile(`${dirPath}${secretfile}`, 'utf-8');
                  outerThis.setEnv(keyPath, raw.replace('\n', ''));
                  watcher.close();

                  success();
                }
              } catch (error) {
                fails(error);
              }
            });
          } catch (error) {
            fails(error);
          }
        });
      } else {
        outerThis.logger.debug(` No secret files were referenced to ${keyPath}.`);
        resolve();
      }
    });
  }

  /**
   * Divides key path of a property.
   *
   * @param {string} field the keys path of a property.
   *
   * @returns an array of the keys path of a property.
   *
   * @private
   */
  static splitPath(field) {
    const sources = field.split('.');
    return sources.length === 1 ? sources : sources
      .reduce((previousValue, currentValue, index) => (
        index === 1 ? [previousValue, currentValue] : [previousValue[0], `${previousValue[1]}.${currentValue}`]
      ));
  }

  /**
   * Gets value of an object property from a key path.
   *
   * @param {string} field the keys path of a property.
   *
   * @returns a value of an object property
   *
   * @private
   */
  getEnv(field) {
    const source = SecretFileHandler.splitPath(field);
    let at = source.shift();
    let data = this.config;
    while (at) {
      if (!Object.hasOwnProperty.bind(data)(at)) {
        return undefined;
      }

      data = data[`${at}`];
      at = source.shift();
    }

    return data;
  }

  /**
   * Sets a value on an object property
   *
   * @param {string} field the keys path of a property.
   * @param {any} value the value
   *
   * @private
   */
  setEnv(field, value) {
    const source = SecretFileHandler.splitPath(field);
    if (source.length === 1) {
      this.config[`${source[0]}`] = value;
    } else if (source.length === 2) {
      if (!this.config[`${source[0]}`]) {
        this.config[`${source[0]}`] = {};
      }
      this.config[`${source[0]}`][`${source[1]}`] = value;
    }
  }
};
