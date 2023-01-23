const { LocalPersistence, Logger } = require('@dojot/microservice-sdk');
const { pipeline, Writable } = require('stream');
const util = require('util');
const pipelineAsync = util.promisify(pipeline);


const logger = new Logger('leveldb-tools');
logger.info = () => {}
const localPersistence = new LocalPersistence.LocalPersistenceManager(logger, true, "./data");

localPersistence.init().then(async () => {
  const managementLevelReadableStream = await localPersistence.createKeyStreamInDisk('managementLevel');
  const managementLevelWritableStream = Writable({
    async write(
      key, encoding, cb,
    ) {

      if(key.toString() != 'managementLevel') {
        console.log(`\t+${key.toString()}`);

        const levelWritableStream = Writable({
          async write(
            key, encoding, cb,
          ) {
            console.log(`\t\t-${key.toString()}`);
            cb();
          },
        });

        const levelReadableStream = await localPersistence.createKeyStreamInDisk(key.toString());
          await pipelineAsync(levelReadableStream, levelWritableStream)
      }
      cb();
    },
  });

  console.log("\n\n+MgmtLevel")
  pipelineAsync(managementLevelReadableStream, managementLevelWritableStream)
    .then(() => console.log('------------------------------'))
    .catch(console.error)
});