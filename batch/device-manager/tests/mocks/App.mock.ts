import { mockDeep } from 'jest-mock-extended';
import { DevicesRepository } from 'src/app/repository';
import { TemplatesServices, DevicesServices } from 'src/app/services/';
import { PrismaUtils } from '../../src/utils/Prisma.utils';

export const AppMock = {
  new() {
    const PrismaUtilsMock = mockDeep<PrismaUtils>();
    const DeviceServiceMock = mockDeep<DevicesServices>();
    const DeviceRepositoryMock = mockDeep<DevicesRepository>();

    return {
      PrismaUtilsMock,
      DeviceServiceMock,
      DeviceRepositoryMock,
    };
  },
};
