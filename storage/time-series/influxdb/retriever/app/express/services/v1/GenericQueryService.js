const { WebUtils } = require('@dojot/microservice-sdk');

module.exports = class GenericQueryService {
  constructor(deviceDataRepository) {
    this.deviceDataRepository = deviceDataRepository;
  }

  async runQuery(org, query) {
    const regex = /from[ ]*[(][ ]*bucket[ ]*:[ ]*"[\w]+"[ ]*\)/gm;
    if (regex.test(query)) {
      throw WebUtils.framework.errorTemplate.BadRequest('The "from" function is determined by dojot');
    }

    const result = await this.deviceDataRepository.runGenericQuery(org, query);

    return result;
  }
};
