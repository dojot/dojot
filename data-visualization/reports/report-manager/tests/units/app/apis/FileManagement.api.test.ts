import { ConfigMock } from 'tests/mocks'
import { FileManagementApi } from 'src/app/apis'

const FAKE_RETURNED_DATA = {
  message: 'message',
}

jest.mock('axios', () => {
  return {
    create: jest.fn(() => ({
      put: jest.fn(() => ({ data: FAKE_RETURNED_DATA })),
      delete: jest.fn(() => ({ data: FAKE_RETURNED_DATA })),
    })),
  }
})

describe('Retriever.api', () => {
  describe('upload', () => {
    it('should upload file', async () => {
      const fileManagementApi = new FileManagementApi(ConfigMock.new())
      const token = '123456'

      const data = await fileManagementApi.upload(
        { file: Buffer.from([]), path: '/tests/test.csv' },
        { token },
      )

      expect(data).toEqual(FAKE_RETURNED_DATA)
      expect(fileManagementApi['api'].put).toBeCalledWith(
        expect.any(String),
        expect.any(Object),
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        },
      )
    })

    it('should avoid duplicating the Bearer prefix on token', async () => {
      const fileManagementApi = new FileManagementApi(ConfigMock.new())
      const tokenWithBearer = 'Bearer 123456'

      const data = await fileManagementApi.upload(
        { file: Buffer.from([]), path: '/tests/test.csv' },
        { token: tokenWithBearer },
      )

      expect(data).toEqual(FAKE_RETURNED_DATA)
      expect(fileManagementApi['api'].put).toBeCalledWith(
        expect.any(String),
        expect.any(Object),
        {
          headers: {
            Authorization: tokenWithBearer,
          },
        },
      )
    })
  })

  describe('delete', () => {
    it('should delete file', async () => {
      const fileManagementApi = new FileManagementApi(ConfigMock.new())

      const token = '123456'
      const path = '/tests/test.csv'
      const data = await fileManagementApi.delete(path, { token })
      const params = new URLSearchParams({ path })

      expect(data).toEqual(FAKE_RETURNED_DATA)
      expect(fileManagementApi['api'].delete).toBeCalledWith(
        expect.stringContaining(`?${params.toString()}`),
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        },
      )
    })

    it('should avoid duplicating the Bearer prefix on token', async () => {
      const fileManagementApi = new FileManagementApi(ConfigMock.new())

      const path = '/tests/test.csv'
      const tokenWithBearer = 'Bearer 123456'
      const params = new URLSearchParams({ path })

      const data = await fileManagementApi.delete(path, {
        token: tokenWithBearer,
      })

      expect(data).toEqual(FAKE_RETURNED_DATA)
      expect(fileManagementApi['api'].delete).toBeCalledWith(
        expect.stringContaining(`?${params.toString()}`),
        {
          headers: {
            Authorization: tokenWithBearer,
          },
        },
      )
    })
  })
})
