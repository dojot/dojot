const {
  WebUtils: {
    KeycloakClientSession,
  },
  LocalPersistence: {
    InputPersister, InputPersisterArgs,
  },
} = require('@dojot/microservice-sdk');
const util = require('util');
const { pipeline, Writable } = require('stream');


const pipelineAsync = util.promisify(pipeline);
// Config InputPersister
const INPUT_CONFIG_TENANTS_PAYLOAD = {
  levels: [
    {
      type: 'static',
      name: 'tenants',
      options: {
        keyEncoding: 'utf8',
        valueEncoding: 'json',
      },
    },
  ],
  frames: [
    {
      level: 0,
      pair: {
        key: {
          type: 'dynamic',
          source: 'tenant.id',
        },
        value: {
          type: 'dynamic',
          source: 'tenant',
        },
      },
    },
  ],
};

class TenantService {
  /**
   * Consumes api that returns tenants data
   *
   * @param {string} tenantsRouteUrl Url for api that returns data about tenants
   */
  constructor(localPersistence, tenantsRouteUrl, dojotClientHttp, keycloakConfig, logger) {
    this.tenantsRouteUrl = tenantsRouteUrl;
    this.dojotClientHttp = dojotClientHttp;
    this.keycloakConfig = keycloakConfig;
    this.logger = logger;
    this.db = localPersistence;

    // create props
    this.inputPersister = new InputPersister(localPersistence, INPUT_CONFIG_TENANTS_PAYLOAD);
    this.tenants = [];
  }

  /**
   * Requires tenant data for API
   *
   * @param {string} tenant the tenant name
   *
   * @returns a list of tenants
   */
  async loadTenants() {
    try {
      await this.requestTenants();
      await this.clearTenantsInDB();
      await this.saveTenants();
    } catch (requestError) {
      this.logger.debug('It was not possible to sync with Tenant service.');
      this.logger.error(requestError);

      try {
        // Processing of data obtained from the database
        const outerThis = this;
        const tenantWritableStream = Writable({
          async write(key, encoding, cb) {
            const tenant = await outerThis.db.get('tenants', key.toString());
            const keycloakSession = await outerThis.initKeycloakSession(tenant);
            outerThis.tenants.push({
              ...tenant,
              session: keycloakSession,
            });
            cb();
          },
        });

        // Get data and pipe
        const tenantReadableStream = await this.db.createKeyStream('tenants');
        await pipelineAsync(
          tenantReadableStream,
          tenantWritableStream,
        );
      } catch (readTenantsError) {
        this.logger.debug('It was not possible to retrieve the tenants');
        this.logger.error(readTenantsError);
      }
    }
  }

  async addNewTenant(tenant) {
    await this.inputPersister.dispatch({
      tenant: {
        id: tenant.id,
        signatureKey: tenant.signatureKey,
      },
    }, InputPersisterArgs.INSERT_OPERATION);
    const keycloakSession = await this.initKeycloakSession(tenant);
    this.tenants.push({
      ...tenant,
      session: keycloakSession,
    });
  }

  async deleteTenant(tenant) {
    await this.inputPersister.dispatch({
      tenant: {
        id: tenant.id,
        signatureKey: tenant.signatureKey,
      },
    }, InputPersisterArgs.DELETE_OPERATION);

    this.tenants = this.tenants.filter((tenantItem) => {
      if (tenantItem.id === tenant.id) {
        tenantItem.session.close();
        return false;
      }

      return true;
    });
  }

  async saveTenants() {
    try {
      this.logger.debug('Clean up tenants sublevel');
      await this.db.clear('tenants');
    } catch (error) {
      this.logger.error(error);
    }

    // eslint-disable-next-line no-restricted-syntax
    for (const tenant of this.tenants) {
      // Write tenants
      // eslint-disable-next-line no-await-in-loop
      await this.inputPersister.dispatch({
        tenant: {
          id: tenant.id,
          signatureKey: tenant.signatureKey,
        },
      }, InputPersisterArgs.INSERT_OPERATION);
    }
  }

  async requestTenants() {
    const response = await this.dojotClientHttp.request(
      {
        url: this.tenantsRouteUrl,
        method: 'GET',
        timeout: 12000,
      },
    );

    // Authenticating to all tenants
    const tenantsPromises = response.data.tenants.map(async (tenant) => {
      const keycloakSession = await this.initKeycloakSession(tenant);

      return {
        ...tenant,
        session: keycloakSession,
      };
    });

    // Waiting for all sessions to start
    this.tenants = await Promise.all(tenantsPromises);
  }

  async initKeycloakSession(tenant) {
    const keycloakSession = new KeycloakClientSession(
      this.keycloakConfig.uri,
      tenant.id,
      {
        grant_type: 'client_credentials',
        client_id: this.keycloakConfig['client.id'],
        client_secret: this.keycloakConfig['client.secret'],
      },
      this.logger,
      {},
    );
    await keycloakSession.start();
    return keycloakSession;
  }

  async clearTenantsInDB() {
    const tenantPromise = this.tenants.map((tenant) => this.inputPersister.dispatch({
      tenant: {
        id: tenant.id,
        signatureKey: tenant.signatureKey,
      },
    }, InputPersisterArgs.DELETE_OPERATION));

    await Promise.all(tenantPromise);
  }
}

module.exports = TenantService;
