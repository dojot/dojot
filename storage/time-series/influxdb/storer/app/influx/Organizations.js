const { InfluxDB } = require('@influxdata/influxdb-client');
const { SetupAPI, OrgsAPI, BucketsAPI } = require('@influxdata/influxdb-client-apis');
const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger('influxdb-storer:influx/Organizations');

/**
 * This class handle with to create organizations, buckets and set Retention.
 * @class
 */
class Organizations {
  /**
   *
   * @param {string} url  Url to access influxdb
   * @param {string} defaultToken Set up initial token
   *                              (This token must have permission to write/read
   *                              in all organizations)
   * @param {string} defaultUser Set up initial user
   * @param {string} defaultPassword Set up initial password
   * @param {string} defaultOrg Organization Name Set up initial Organization
   * @param {string} defaultBucket Bucket Name for all buckets created
   * @param {Number} retentionPeriodHrs Retention Period in hours
   *                                    for all buckets created (zero is infinite retention)
   */
  constructor(url,
    defaultToken,
    defaultUser,
    defaultPassword,
    defaultOrg,
    defaultBucket,
    retentionPeriodHrs) {
    logger.debug('constructor:');
    logger.debug(`constructor: url=${url}`);
    logger.debug(`constructor: defaultToken=${defaultToken}`);
    logger.debug(`constructor: defaultUser=${defaultUser}`);
    logger.debug(`constructor: defaultPassword=${defaultPassword}`);
    logger.debug(`constructor: defaultBucket=${defaultBucket}`);
    logger.debug(`constructor: retentionPeriodHrs=${retentionPeriodHrs}`);

    const influxDB = new InfluxDB({ url, token: defaultToken });
    this.setupApi = new SetupAPI(influxDB);
    this.orgsApi = new OrgsAPI(influxDB);
    this.bucketsAPI = new BucketsAPI(influxDB);

    this.defaultToken = defaultToken;
    this.defaultUser = defaultUser;
    this.defaultPassword = defaultPassword;
    this.defaultOrg = defaultOrg;
    this.defaultBucket = defaultBucket;
    this.retentionPeriodHrs = retentionPeriodHrs;
    if (this.retentionPeriodHrs < 0) {
      this.retentionPeriodHrs = 0;
    }
  }

  /**
   * Check if some org exists
   *
   * @param {String} org Organization Name
   * @returns {Promise.<boolean>} true if exist or false otherwise
   *
   * @throws If it is not possible to check if the organization exists in case an error occurs
   */
  async hasOrg(org) {
    logger.debug(`hasOrg: Has ${org} org?`);
    try {
      const organization = await this.getOrgInfo(org);
      if (organization && organization.id) {
        logger.debug(`hasOrg: Has ${org} org? true`);
        return true;
      }
    } catch (e) {
      logger.error(`hasOrg: Some error when check if ${org} exist`, e);
      throw new Error('Cannot check if org exist');
    }
    logger.debug(`hasOrg: Has the ${org}? false`);
    return false;
  }

  /**
   * Check if the default bucket  exists in a org
   *
   * @param {String} org Organization Name
   * @returns  {Promise.<boolean>} true if exist or false otherwise
   *
   * @throws If Cannot check if default bucket exists in org
   */
  async hasDefaultBucketInOrg(org) {
    logger.debug(`hasDefaultBucketInOrg: Has default bucket in ${org} org?`);
    try {
      const buckets = await this.bucketsAPI.getBuckets({ org, name: this.defaultBucket });
      if (buckets && buckets.buckets && buckets.buckets.length) {
        logger.debug(`hasDefaultBucketInOrg: Has default bucket in ${org} org? true`);
        return true;
      }
      logger.debug(`hasDefaultBucketInOrg: Has default bucket in ${org} org? false`);
      return false;
    } catch (e) {
      if (e.statusMessage === 'Not Found') {
        logger.debug(`hasDefaultBucketInOrg: Has default bucket in ${org} org? false`);
        return false;
      }
      logger.error(`hasDefaultBucketInOrg: Some error when check if default bucket in ${org} exist`, e);
      throw new Error('Cannot check if default bucket exist in org');
    }
  }

  /**
   * Get information about a Organization
   *
   * @param {String} org Organization Name
   *
   * @returns {Promise.<Object>}  A object with Organization infos
   *
   * @throws If Cannot get info about org
   */
  async getOrgInfo(org) {
    logger.debug(`getOrgInfo: Getting info for  ${org} org...`);
    try {
      const organizations = await this.orgsApi.getOrgs({ org });
      if (organizations && organizations.orgs && organizations.orgs.length) {
        logger.debug(`getOrgInfo: Info from ${org} org.`, organizations.orgs[0]);
        return organizations.orgs[0];
      }
      return null;
    } catch (e) {
      if (e.statusMessage === 'Not Found') {
        logger.debug(`getOrgInfo: ${org} org doest exist.`);
        return null;
      }
      logger.error(`getOrgInfo: Some error when get info about ${org} org`, e);
      throw new Error('Cannot get info about org');
    }
  }

  /**
   * Delete a organization
   *
   * @param {String} org Organization to delete
   *
   */
  async deleteOrg(org) {
    logger.debug(`deleteOrg: Deleting  ${org} org...`);
    try {
      const orgInfo = await this.getOrgInfo(org);
      if (orgInfo) {
        const orgID = orgInfo.id;
        await this.orgsApi.deleteOrgsID({ orgID });
        logger.info(`deleteOrg: The ${org} org was deleted.`);
      } else {
        logger.warn(`deleteOrg: The ${org} org doest exist.`);
      }
    } catch (e) {
      logger.error(`deleteOrg: Some error when try to delete a ${org} org`, e);
      // not throw in deletes
    }
  }

  /**
   *  Set up initial user, org and bucket
   *  Post an onboarding request to set up initial user, org and bucket.
   *
   *  @throws If Cannot onboard initial Org
   */
  async initOnboarding() {
    logger.debug(`initOnboarding: Init default ${this.defaultOrg} org...`);
    try {
      const { allowed } = await this.setupApi.getSetup();
      if (allowed) {
        const res = await this.setupApi.postSetup({
          body: {
            org: this.defaultOrg,
            bucket: this.defaultBucket,
            username: this.defaultUser,
            password: this.defaultPassword,
            token: this.defaultToken,
            retentionPeriodHrs: this.retentionPeriodHrs,
          },
        });
        logger.info(`initOnboarding: The default ${this.defaultOrg} org was created`);
        logger.debug('initOnboarding:', res);
      } else {
        logger.warn(`initOnboarding: Default ${this.defaultOrg} org already onboard.`);
      }
    } catch (e) {
      logger.error(`initOnboarding: Some error when try to onboarding a new ${this.defaultOrg} org.`, e);
      throw new Error('Cannot onboard initial Org');
    }
  }

  /**
   * Create a organization with a default bucket
   *
   * @param {String} org New organization name
   *
   * @throws Cannot create an org and a bucket
   */
  async createOrgWithDefaultBucket(org) {
    logger.debug(`createOrgWithDefaultBucket: Creating the ${org} org...`);
    try {
      const orgInfo = await this.getOrgInfo(org);
      if (!orgInfo) {
        logger.debug(`createOrgWithDefaultBucket: Creating a new ${org} org`);
        const { id: orgID } = await this.orgsApi.postOrgs({ body: { name: org } });
        logger.info(`createOrgWithDefaultBucket: The ${org} org with ${orgID} id was created`);
        await this.createBucket(orgID);
      } else {
        logger.warn(`createOrgWithDefaultBucket: The ${org} org already exist.`);
        // check if bucket default exist else create
        const hasDefaultBucketInOrg = await this.hasDefaultBucketInOrg(org);
        logger.debug(`createOrgWithDefaultBucket: Check if ${org} org has the default bucket`);
        if (!hasDefaultBucketInOrg) {
          logger.debug(`createOrgWithDefaultBucket: The ${org} org hasn't the default bucket`);
          const orgID = orgInfo.id;
          await this.createBucket(orgID);
        }
      }
    } catch (e) {
      logger.error(`createOrgWithDefaultBucket: Some error when try to create a org ${org} and a bucket`, e);
      throw new Error('Cannot create a org and a bucket');
    }
  }

  /**
   * Create the default bucket in a org
   *
   * @param {String} orgID Organization id
   *
   * @throws Cannot create a org and a bucket
   */
  async createBucket(orgID) {
    try {
      const retentionRules = [];
      if (this.retentionPeriodHrs > 0) {
        retentionRules.push({ type: 'expire', everySeconds: this.retentionPeriodHrs * 3600 });
      }
      logger.debug(`createBucket: Creating a new ${this.defaultBucket} bucket in ${orgID} org. With retentionRules=`, retentionRules);
      const res = await this.bucketsAPI.postBuckets({
        body: {
          name: this.defaultBucket,
          orgID,
          retentionRules,
        },
      });
      logger.info(`createBucket: The ${this.defaultBucket} bucket in ${orgID} org was created`);
      logger.debug('createBucket:', res);
    } catch (e) {
      logger.error('createBucket: Some error when try to create a bucket', e);
      throw new Error('Cannot create a bucket');
    }
  }
}

module.exports = Organizations;
