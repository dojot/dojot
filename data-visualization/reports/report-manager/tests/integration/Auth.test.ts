import { Server } from 'http'

import request from 'supertest'

import { AppSetup, AuthSetup } from './setup'

jest.mock('@prisma/client', () => ({
  PrismaClient: jest.fn().mockImplementation(() => ({
    $disconnect: jest.fn(),
  })),
}))

jest.mock('bull')

describe('Auth', () => {
  let server: Server
  const ENDPOINT = '/devices'

  beforeAll(async () => {
    server = await AppSetup.initApp()
  })

  afterAll(() => {
    server.close()
  })

  it('should return error 401 when there is no authorization header', async () => {
    const response = await request(server).post(ENDPOINT)
    expect(response.status).toBe(401)
  })

  it('should return error 401 when access token is invalid', async () => {
    const INVALID_ACCESS_TOKEN = 'INVALID_ACCESS_TOKEN'

    const response = await request(server)
      .post(ENDPOINT)
      .set('Authorization', `Bearer ${INVALID_ACCESS_TOKEN}`)

    expect(response.status).toBe(401)
  })

  it('should return error 401 when Bearer is missing', async () => {
    const response = await request(server)
      .post(ENDPOINT)
      .set('Authorization', AuthSetup.signJWT())

    expect(response.status).toBe(401)
  })
})
