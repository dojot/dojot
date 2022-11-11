import { FileManagementApi } from 'src/app/apis'
import { DojotHttpClientMock } from 'tests/mocks'

describe('FileManagement.api', () => {
  const FAKE_RETURNED_DATA = {
    message: 'message',
  }

  const FAKE_DOJOT_CLIENT_RESPONSE = { data: FAKE_RETURNED_DATA } as never

  describe('upload', () => {
    it('should upload file', async () => {
      const token = '123456'

      const FakeDojotClient = DojotHttpClientMock.new()
      FakeDojotClient.request.mockResolvedValue(FAKE_DOJOT_CLIENT_RESPONSE)
      const fileManagementApi = new FileManagementApi(FakeDojotClient)

      const data = await fileManagementApi.upload(
        { file: Buffer.from([]), path: '/tests/test.csv' },
        { token },
      )

      expect(data).toEqual(FAKE_RETURNED_DATA)
      expect(fileManagementApi['api'].request).toBeCalledWith({
        method: 'PUT',
        url: expect.any(String),
        data: expect.any(Object),
        headers: expect.objectContaining({
          // Authorization header is lower case here
          authorization: `Bearer ${token}`,
        }),
      })
    })

    it('should avoid duplicating the Bearer prefix on token', async () => {
      const tokenWithBearer = 'Bearer 123456'

      const FakeDojotClient = DojotHttpClientMock.new()
      FakeDojotClient.request.mockResolvedValue(FAKE_DOJOT_CLIENT_RESPONSE)
      const fileManagementApi = new FileManagementApi(FakeDojotClient)

      const data = await fileManagementApi.upload(
        { file: Buffer.from([]), path: '/tests/test.csv' },
        { token: tokenWithBearer },
      )

      expect(data).toEqual(FAKE_RETURNED_DATA)
      expect(fileManagementApi['api'].request).toBeCalledWith({
        method: 'PUT',
        url: expect.any(String),
        data: expect.any(Object),
        headers: expect.objectContaining({
          // Authorization header is lower case here
          authorization: tokenWithBearer,
        }),
      })
    })
  })

  describe('delete', () => {
    it('should delete file', async () => {
      const FakeDojotClient = DojotHttpClientMock.new()
      FakeDojotClient.request.mockResolvedValue(FAKE_DOJOT_CLIENT_RESPONSE)
      const fileManagementApi = new FileManagementApi(FakeDojotClient)

      const token = '123456'
      const path = '/tests/test.csv'
      const data = await fileManagementApi.delete(path, { token })
      const params = new URLSearchParams({ path })

      expect(data).toEqual(FAKE_RETURNED_DATA)
      expect(fileManagementApi['api'].request).toBeCalledWith({
        method: 'DELETE',
        url: expect.stringContaining(`?${params.toString()}`),
        headers: {
          Authorization: `Bearer ${token}`,
        },
      })
    })

    it('should avoid duplicating the Bearer prefix on token', async () => {
      const FakeDojotClient = DojotHttpClientMock.new()
      FakeDojotClient.request.mockResolvedValue(FAKE_DOJOT_CLIENT_RESPONSE)
      const fileManagementApi = new FileManagementApi(FakeDojotClient)

      const path = '/tests/test.csv'
      const tokenWithBearer = 'Bearer 123456'
      const params = new URLSearchParams({ path })

      const data = await fileManagementApi.delete(path, {
        token: tokenWithBearer,
      })

      expect(data).toEqual(FAKE_RETURNED_DATA)
      expect(fileManagementApi['api'].request).toBeCalledWith({
        method: 'DELETE',
        url: expect.stringContaining(`?${params.toString()}`),
        headers: {
          Authorization: tokenWithBearer,
        },
      })
    })
  })
})
