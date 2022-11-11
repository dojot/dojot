import { Server } from 'http'

import request from 'supertest'

import { APP_ERRORS } from 'src/app/constants'

import { AppSetup, AuthSetup } from './setup'

const REPORT_ID = 'id'
const FAKE_REPORT = { id: REPORT_ID }
const FAKE_REPORTS = [FAKE_REPORT]

const FAKE_REPORT_WITH_FILE = {
  id: REPORT_ID,
  file: { path: 'path/to/file' },
}

jest.mock('@prisma/client', () => ({
  PrismaClient: jest.fn().mockImplementation(() => ({
    $disconnect: jest.fn(),
    report: {
      count: jest.fn().mockResolvedValue(FAKE_REPORTS.length),
      findMany: jest.fn().mockResolvedValue(FAKE_REPORTS),
      findUnique: jest.fn().mockImplementation((data) => {
        if (data.where.id === 'id') return FAKE_REPORT
        return null
      }),
      delete: jest.fn().mockImplementation((data) => {
        if (data.where.id === 'id') return FAKE_REPORT_WITH_FILE
        return null
      }),
    },
  })),
}))

jest.mock('bull')

describe('Report.routes', () => {
  let server: Server

  beforeAll(async () => {
    server = await AppSetup.initApp()
  })

  afterAll(() => {
    server.close()
  })

  describe('findMany', () => {
    it('should return entire list of reports', async () => {
      const response = await request(server)
        .get('/reports')
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(200)
      expect(response.body).toEqual({
        reports: FAKE_REPORTS,
        pagination: {
          total: FAKE_REPORTS.length,
        },
      })
    })

    it('should return a paginated list of reports', async () => {
      const response = await request(server)
        .get('/reports')
        .query({ page: 2, pageSize: 15 })
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(200)
      expect(response.body).toEqual({
        reports: FAKE_REPORTS,
        pagination: {
          page: 2,
          pageSize: 15,
          total: FAKE_REPORTS.length,
        },
      })
    })

    it('should search reports by name', async () => {
      const response = await request(server)
        .get('/reports')
        .query({ name: 'Report' })
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(200)
      expect(response.body).toEqual(
        expect.objectContaining({
          reports: FAKE_REPORTS,
        }),
      )
    })

    it('should return error 400 when page query param is invalid', async () => {
      const response = await request(server)
        .get('/reports')
        .query({ page: NaN, pageSize: 15 })
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(400)
      expect(response.body).toMatchObject({ type: APP_ERRORS.VALIDATION })
    })

    it('should return error 400 when pageSize query param is invalid', async () => {
      const response = await request(server)
        .get('/reports')
        .query({ page: 2, pageSize: NaN })
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(400)
      expect(response.body).toMatchObject({ type: APP_ERRORS.VALIDATION })
    })

    it('should return error 400 when name query param is invalid', async () => {
      const response = await request(server)
        .get('/reports')
        .query({ name: { invalid: true } })
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(400)
      expect(response.body).toMatchObject({ type: APP_ERRORS.VALIDATION })
    })
  })

  describe('findById', () => {
    it('should return a single report', async () => {
      const response = await request(server)
        .get(`/reports/${REPORT_ID}`)
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(200)
      expect(response.body).toEqual(FAKE_REPORT)
    })

    it('should return error 404 when report was not found by id', async () => {
      const NON_EXISTENT_ID = 'NON_EXISTENT_ID'

      const response = await request(server)
        .get(`/reports/${NON_EXISTENT_ID}`)
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(404)
    })
  })

  describe('delete', () => {
    it('should delete a report', async () => {
      const response = await request(server)
        .delete(`/reports/${REPORT_ID}`)
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(200)
      expect(response.body).toEqual(FAKE_REPORT_WITH_FILE)
    })

    it('should return error 404 when report was not found to delete', async () => {
      const NON_EXISTENT_ID = 'NON_EXISTENT_ID'

      const response = await request(server)
        .get(`/reports/${NON_EXISTENT_ID}`)
        .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)

      expect(response.status).toBe(404)
    })
  })
})
