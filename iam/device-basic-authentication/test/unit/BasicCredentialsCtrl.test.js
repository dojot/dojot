jest.mock('mongoose');

const mongoose = require('mongoose');

mongoose.Schema.mockImplementation(() => ({
  pre: jest.fn(),
  methods: {
    comparePassword: jest.fn(),
  },
}));

mongoose.model.mockImplementation((model, schema) => {
  const mongooseModelInstance = {
    model, schema,
  };

  // constructor function
  const modelRef = jest.fn().mockImplementation(() => mongooseModelInstance);

  // class functions
  modelRef.findOne = jest.fn(() => modelRef);
  modelRef.find = jest.fn(() => modelRef);
  modelRef.deleteMany = jest.fn(() => modelRef);
  modelRef.deleteOne = jest.fn(() => modelRef);
  modelRef.create = jest.fn(() => modelRef);

  return modelRef;
});

const mockBasicCredentials = require('../../app/db/models/BasicCredentials');
const mockTenant = require('../../app/db/models/Tenant');

const BasicCredentialsCtrl = require('../../app/db/controllers/BasicCredentialsCtrl');

describe('BasicCredentialsCtrl', () => {
  let basicCredentialsCtrl = null;

  beforeEach(() => {
    basicCredentialsCtrl = new BasicCredentialsCtrl(mockBasicCredentials, mockTenant);
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create constructor', () => {
      expect(basicCredentialsCtrl.basicCredentials).toBeDefined();
      expect(basicCredentialsCtrl.tenant).toBeDefined();
    });
  });

  describe('create', () => {
    it('should save tenant create credentials and returned', async () => {
      mockTenant.findOne.mockReturnValue(null);
      await basicCredentialsCtrl.create('tenant1', '123abc');
      expect(mockTenant.create).toHaveBeenCalled();
      expect(mockBasicCredentials.create).toHaveBeenCalled();
    });

    it('should just create the credentials and returned', async () => {
      mockTenant.findOne.mockReturnValue('tenant1');
      await basicCredentialsCtrl.create('tenant1', '123abc');
      expect(mockTenant.create).not.toHaveBeenCalled();
      expect(mockBasicCredentials.create).toHaveBeenCalled();
    });
  });

  describe('authentication', () => {
    it('should return the status of the password comparison with the hash saved in the database', async () => {
      mockBasicCredentials.findOne.mockReturnValue({ comparePassword: jest.fn(() => true) });
      const credential = await basicCredentialsCtrl.authentication('tenant1@123abc', 'Ph0U*%hgf$5vbnK');
      expect(mockBasicCredentials.findOne).toHaveBeenCalled();
      expect(credential).toBeTruthy();
    });
  });
  describe('findAllDevicesFromTenant', () => {
    it('should return a list of all devices credentialed with that tenant', async () => {
      mockBasicCredentials.find.mockResolvedValue([{ deviceId: 'device1' }, { deviceId: 'device2' }, { deviceId: 'device3' }]);
      const credentilsDevices = await basicCredentialsCtrl.findAllDevicesFromTenant('tenant1');
      expect(mockBasicCredentials.find).toHaveBeenCalled();
      expect(credentilsDevices.sort()).toEqual(['device1', 'device2', 'device3'].sort());
    });
  });
  describe('removeAllFromTenant', () => {
    it('should remove all saved credentials containing that tenant', async () => {
      await basicCredentialsCtrl.removeAllFromTenant('tenant1');
      expect(mockBasicCredentials.deleteMany).toHaveBeenCalled();
      expect(mockTenant.deleteOne).toHaveBeenCalled();
    });
  });
  describe('remove', () => {
    it('should remove that credential', async () => {
      await basicCredentialsCtrl.remove('tenant1', 'device1');
      expect(mockBasicCredentials.deleteOne).toHaveBeenCalled();
    });
  });
});
