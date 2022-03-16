const AcceptHeaderHelper = require('../../app/express/helpers/AcceptHeaderHelper');

describe('AcceptHeaderHelper', () => {
  it('Should accept the request', () => {
    const mockReq = {
      accepts: () => true,
    };

    const returnValue = AcceptHeaderHelper.getAcceptableType(mockReq);
    expect(returnValue).toBeTruthy();
  });

  it('Should not accept the request', () => {
    const mockReq = {
      headers: {
        accept: 'mimitype',
      },
      accepts: () => false,
    };
    let error;

    try {
      AcceptHeaderHelper.getAcceptableType(mockReq);
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON.error).toEqual('This server does not support mimitype');
  });
});
