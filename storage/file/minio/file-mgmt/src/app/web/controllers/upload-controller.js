const { WebUtils } = require('@dojot/microservice-sdk');

module.exports = class UploadController {
  // eslint-disable-next-line no-unused-vars
  upload = async (req, res) => {
    await WebUtils.framework.errorTemplate.MethodNotAllowed('Not Allow', 'Not Allow');
  }
};
