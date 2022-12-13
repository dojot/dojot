import { Server } from 'http'

import request from 'supertest'

import { CreateDeviceReportDto } from 'src/app/dto'
import { APP_ERRORS, REPORT_FORMATS } from 'src/app/constants'

import { AppSetup, AuthSetup } from './setup'

const TEST_ID = 'TEST_ID'

jest.mock('@prisma/client', () => ({
  PrismaClient: jest.fn().mockImplementation(() => ({
    $disconnect: jest.fn(),
    reportType: {
      findUnique: jest.fn().mockResolvedValue({}),
    },
    report: {
      create: jest.fn().mockResolvedValue({ id: TEST_ID }),
    },
  })),
}))

jest.mock('bull')

describe('Device.routes', () => {
  let server: Server

  beforeAll(async () => {
    server = await AppSetup.initApp()
  })

  afterAll(() => {
    server.close()
  })

  const createDeviceReportDto: CreateDeviceReportDto = {
    name: 'Report name',
    format: REPORT_FORMATS.CSV,
    devices: [
      {
        id: 'abc123',
        label: 'Device name',
        attrs: [
          {
            id: 1,
            label: 'Attr 1',
            type: 'dynamic',
            valueType: 'string',
          },
        ],
      },
    ],
  }

  it('should return the report id when create a report', async () => {
    const response = await request(server)
      .post('/devices')
      .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)
      .send(createDeviceReportDto)

    expect(response.status).toBe(200)
    expect(response.body).toEqual({ id: TEST_ID })
  })

  it('should return error 400 when send invalid data', async () => {
    const response = await request(server)
      .post('/devices')
      .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)
      .send({})

    expect(response.status).toBe(400)
    expect(response.body).toMatchObject({ type: APP_ERRORS.VALIDATION })
  })
})
