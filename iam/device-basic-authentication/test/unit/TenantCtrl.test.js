jest.mock('mongoose');

const mongoose = require('mongoose');

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

const mockTenant = require('../../app/db/models/Tenant');

const TenantCtrl = require('../../app/db/controllers/TenantCtrl');

describe('TenantCtrl', () => {
  let tenantCtrl = null;

  beforeEach(() => {
    tenantCtrl = new TenantCtrl(mockTenant);
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create constructor', () => {
      expect(tenantCtrl.tenant).toBeDefined();
    });
  });

  describe('create', () => {
    it('should create tenant', async () => {
      await tenantCtrl.create('tenant1');
      expect(mockTenant.create).toHaveBeenCalled();
    });
  });

  describe('findAll', () => {
    it('should return all tenants', async () => {
      mockTenant.find.mockResolvedValue([{ tenant: 'tenant1' }]);
      // await expect(mongoClient.connect()).resolves.toBeUndefined();
      await tenantCtrl.findAll();
      expect(mockTenant.find).toHaveBeenCalled();
    });
  });

  describe('remove', () => {
    it('should remove tenants', async () => {
      await tenantCtrl.remove('tenant1');
      expect(mockTenant.deleteOne).toHaveBeenCalled();
    });
  });
});
