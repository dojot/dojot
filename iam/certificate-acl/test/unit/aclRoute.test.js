/* eslint-disable no-unused-vars */
const aclRoute = require('../../app/server/express/aclRoute');

const mockQueryOwnerByFingerprint = (fingerprint) => new Promise((resolve, reject) => {
  resolve({ fingerprint });
});

describe('ACLRoute', () => {
  it('Should return the object route', async () => {
    const route = aclRoute(mockQueryOwnerByFingerprint);

    expect.assertions(6);
    expect(route.mountPoint).toEqual('/internal/api/v1');
    expect(route.name).toEqual('acl-fingerprint-route');
    expect(route.path).toEqual(['/acl-entries/:fingerprint']);
    expect(route.handlers.length).toEqual(1);

    const request = { params: { fingerprint: 'fingerprint' } };
    const response = {
      status: (code) => {
        expect(code).toEqual(200);
        return response;
      },
      json: (value) => {
        expect(value).toEqual({ fingerprint: 'fingerprint' });
      },
    };

    await route.handlers[0].middleware[0](request, response);
  });
});
