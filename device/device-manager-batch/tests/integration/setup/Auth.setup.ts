import { sign } from 'jsonwebtoken';
import { CERTIFICATE, PRIVATE_KEY } from './Constants.setup';

export const signJWT = (tenant = 'test'): string => {
  return sign({ iss: `auth/realms/${tenant}` }, PRIVATE_KEY, {
    expiresIn: 200,
    header: { alg: 'RS512' },
  });
};

export const getTenantInfo = (tenant = 'test') => {
  return {
    id: tenant,
    sigKey: {},
    signatureKey: {
      use: 'SIG',
      algorithm: 'RS512',
      publicKey: 'publicKey',
      certificate: CERTIFICATE,
    },
  };
};
