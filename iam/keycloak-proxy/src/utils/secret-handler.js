/* eslint-disable security/detect-non-literal-fs-filename */
const fs = require('fs');

module.exports = class SecretHandler {
  constructor(config, logger) {
    this.config = config;
    this.logger = logger;
  }

  handleCollection(keyPaths) {
    const outerThis = this;
    return new Promise((resolve, reject) => {
      const keyPathsResolved = [];
      this.logger.info('Loading secrets...');
      keyPaths.forEach((keyPath) => {
        outerThis.handle(keyPath).then(() => {
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

  handle(keyPath) {
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

        fs.promises.readFile(`/secrets/${secretfile}`, 'utf-8').then((raw) => {
          outerThis.logger.debug('Secret file found');
          
          outerThis.setEnv(keyPath, raw.replace('\n', ''));
          success();
        }).catch(() => {
          outerThis.logger.debug('Waiting for secret file');
          try {
            const watcher = fs.watch('/secrets/', { interval: 60 }, async (type, filename) => {
              try {
                if (type === 'rename' && filename === secretfile) {
                  outerThis.logger.debug('Secret file found');

                  const raw = await fs.promises.readFile(`/secrets/${secretfile}`, 'utf-8');
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

  static splitPath(field) {
    const sources = field.split('.');
    return sources.length === 1 ? sources : sources
      .reduce((previousValue, currentValue, index) => (
        index === 1 ? [previousValue, currentValue] : [previousValue[0], `${previousValue[1]}.${currentValue}`]
      ));
  }  

  getEnv(field) {
    const source = SecretHandler.splitPath(field);
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

  setEnv(field, value) {
    const source = SecretHandler.splitPath(field);
    if (source.length === 1) {
      this.config[`${source[0]}`] = value;
    } else if (source.length === 2) {
      this.config[`${source[0]}`] = {};
      this.config[`${source[0]}`][`${source[1]}`] = value;
    }   
  }
};
