const createAxios = require('./createAxios');


class CertificateAclService {
  /**
   * Consumes api that returns tenant and device data
   *
   * @param {string} certificateAclUrl Url for api that returns data about acl entries
   */
  constructor(certificateAclRouteUrl) {
    this.certificateAclRouteUrl = certificateAclRouteUrl;
    this.axios = createAxios();
  }

  /**
   * Requires tenant and device data from acl entries
   *
   * @param {string} fingerprint256 the fingerprint256
   *
   * @return tenant and device
   */
  async getAclEntries(fingerprint256) {
    const messageKey = await this.axios.get(`${this.certificateAclRouteUrl}/${fingerprint256}`);
    return messageKey.data;
  }
}

module.exports = CertificateAclService;
