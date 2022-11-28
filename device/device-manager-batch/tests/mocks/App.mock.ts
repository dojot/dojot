import { mockDeep } from 'jest-mock-extended';
import {
  DevicesRepository,
  TemplatesRepository,
  AttrsRepository,
} from 'src/app/repository';
import { DevicesServices, TemplatesServices } from 'src/app/services/';
import { PrismaUtils } from '../../src/utils/Prisma.utils';

export const AppMock = {
  new() {
    const PrismaUtilsMock = mockDeep<PrismaUtils>();
    const DeviceServiceMock = mockDeep<DevicesServices>();
    const DeviceRepositoryMock = mockDeep<DevicesRepository>();
    const TemplatesRepositoryMock = mockDeep<TemplatesRepository>();
    const TemplatesServiceMock = mockDeep<TemplatesServices>();
    const AttrsRepositoryMock = mockDeep<AttrsRepository>();

    return {
      PrismaUtilsMock,
      DeviceServiceMock,
      DeviceRepositoryMock,
      TemplatesRepositoryMock,
      TemplatesServiceMock,
      AttrsRepositoryMock,
    };
  },
};
