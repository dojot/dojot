const request = require('supertest');
const { token } = require('../util.test');

const certificateModel = global.container.resolve('certificateModel');
certificateModel.model = {
  findOneAndUpdate: jest.fn().mockReturnThis(),
  maxTimeMS: jest.fn().mockReturnThis(),
  exec: jest.fn().mockResolvedValue(),
};

const framework = container.resolve('framework');

const req = request(framework);

describe('X509 Certificates - PATCH integrations', () => {
  it('should change ownership',
    () => {
      const fingerprint = '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD';
      const queryResult = {
        fingerprint,
      };
      certificateModel.model.exec.mockResolvedValue(queryResult);

      return req.patch(`/api/v1/certificates/${fingerprint}`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          belongsTo: {
            application: 'kafka-consumer',
          },
        })
        .expect(204)
        .then((res) => {
          expect(res.body).toEqual({});
        });
    });

  it('should not find any certificate to patch',
    () => {
      const fingerprint = '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD';
      certificateModel.model.exec.mockResolvedValue(null);

      return req.patch(`/api/v1/certificates/${fingerprint}`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          belongsTo: {
            application: 'kafka-consumer',
          },
        })
        .expect(404)
        .then((res) => {
          expect(res.body).toEqual({
            message: `No records found for the following parameters: {"fingerprint":"${fingerprint}","tenant":"admin"}`,
          });
        });
    });
});
