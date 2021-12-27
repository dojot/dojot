const fs = require('fs').promises;
const { CronJob } = require('cron');
const { Logger, ConfigManager } = require('@dojot/microservice-sdk');

const {
  app: configApp,
} = ConfigManager.getConfig('CERT_SC');

const logger = new Logger(`cert-sc-${configApp['sidecar.to']}:Utils`);

/**
 * Create a new CronJob to a function {cb} based in {cronTime} passed.
 * The function cb will be executed immediately
 *
 *
 * @param cb {Function} cb The function to fire at the specified time.
 * @param cronTime {String} cronTime the time to fire off your job.
 *                 This can be in the form of cron syntax
 *                 or a JS ```Date``` object.
 * @param runOnInit {Boolean} runOnInit This will immediately fire your ```cb```
 *                  function as soon as the requisite initialization
 *                  has happened. This option is set to ```true```
 *
 * @throws Cannot create the cron job
 *
 * @returns {cron.CronJob}
 */
const cronJob = (
  cb, cronTime, runOnInit = false,
) => {
  logger.debug(`cronJob: Creating a cronJob cronTime=${cronTime}`);
  try {
    const job = new CronJob({
      cronTime,
      onTick: cb,
      runOnInit,
    });
    job.start();
    logger.info(`cronJob: Created a cronJob cronTime=${cronTime}!`);
    return job;
  } catch (e) {
    logger.error('cronJob:', e);
    throw e;
  }
};

/**
 * Create a new file with some content
 *
 * @param {String} path Path to create the file
 * @param {String} content Content for the new file
 *
 * @throws Cannot create the file
 */
const createFile = async (path, content) => {
  logger.debug(`createFile: Creating a file=${path}`);
  try {
    // eslint-disable-next-line security/detect-non-literal-fs-filename
    await fs.writeFile(path, content);
    logger.info(`createFile: ${path} is created!`);
  } catch (e) {
    logger.error('createFile:', e);
    throw e;
  }
};

/**
 * Create a new dir recursive from a path
 *
 * @param {String} path Path to create the new dir
 *
 * @throws Cannot create the dir
 */
const createDir = async (path) => {
  logger.debug(`createDir: Creating a dir=${path}`);
  try {
    // check if dir exist
    await fs.access(path).catch(async () => {
      // if dir doest exist create it
      // eslint-disable-next-line security/detect-non-literal-fs-filename
      await fs.mkdir(path, { recursive: true });
    });
  } catch (e) {
    logger.error('createDir:', e);
    throw e;
  }
};

/**
 * Delete a dir
 *
 * @param {String} path Path to delete the dir
 *
 * @throws Cannot delete the dir
 */
const deleteDir = async (path) => {
  logger.debug(`deleteDir: Deleting a dir=${path}`);
  try {
    // eslint-disable-next-line security/detect-non-literal-fs-filename
    await fs.rmdir(path, { recursive: true });
    logger.info(`deleteDir: ${path} is deleted!`);
  } catch (e) {
    logger.error('deleteDir:', e);
    throw e;
  }
};

/**
 * Delete a file
 *
 * @param {String} path Path to delete the file
 *
 * @throws Cannot delete the file
 */
const deleteFile = async (path) => {
  logger.debug(`deleteFile: Deleting a file=${path}`);
  try {
    // eslint-disable-next-line security/detect-non-literal-fs-filename
    await fs.unlink(path);
    logger.info(`deleteFile: ${path} is deleted!`);
  } catch (e) {
    logger.error('deleteFile:', e);
    throw e;
  }
};

/**
 * Creates the configuration filename in the format `<path>/<filename>`.
 *
 * @param {string} filename
 * @param {string} path

 * @returns {string} the full path in the format `<path>/<filename>`
 */
const createFilename = (filename, path) => {
  try {
    const newPath = path.replace(/\/$/, '');
    return `${newPath}/${filename.toLowerCase()}`;
  } catch (e) {
    logger.error('createFilename:', e);
    throw e;
  }
};

module.exports = {
  createFile, deleteFile, createDir, cronJob, createFilename, deleteDir,
};
