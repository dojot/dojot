import { RetrieverApi } from 'src/app/apis'
import { DojotHttpClientMock } from 'tests/mocks'

describe('Retriever.api', () => {
  const FAKE_RETURNED_DATA = {
    data: {
      ts: new Date().toISOString(),
      attrs: [
        { label: 'one', value: 1 },
        { label: 'two', value: 2 },
      ],
    },
    paging: {
      current: {
        number: 1,
        url: 'url',
      },
    },
  }

  const FAKE_DOJOT_CLIENT_RESPONSE = { data: FAKE_RETURNED_DATA } as never

  it('should get device data with no filters', async () => {
    const FakeDojotClient = DojotHttpClientMock.new()
    FakeDojotClient.request.mockResolvedValue(FAKE_DOJOT_CLIENT_RESPONSE)
    const retrieverApi = new RetrieverApi(FakeDojotClient)

    const token = '123456'
    const data = await retrieverApi.getDeviceData('id', { token })

    expect(data).toEqual(FAKE_RETURNED_DATA)
    expect(retrieverApi['api'].request).toBeCalledWith({
      method: 'GET',
      url: expect.any(String),
      headers: {
        Authorization: `Bearer ${token}`,
      },
    })
  })

  it('should get device data using filters', async () => {
    const FakeDojotClient = DojotHttpClientMock.new()
    FakeDojotClient.request.mockResolvedValue(FAKE_DOJOT_CLIENT_RESPONSE)
    const retrieverApi = new RetrieverApi(FakeDojotClient)

    const token = '123456'
    const filters = {
      page: 2,
      limit: 100,
      order: 'desc' as const,
      dateTo: new Date().toISOString(),
      dateFrom: new Date().toISOString(),
    }

    // All params below must be in the correct order
    const params = new URLSearchParams({
      dateFrom: filters.dateFrom,
      dateTo: filters.dateTo,
      limit: String(filters.limit),
      page: String(filters.page),
      order: filters.order,
    })

    const data = await retrieverApi.getDeviceData('id', { token }, filters)

    expect(data).toEqual(FAKE_RETURNED_DATA)
    expect(retrieverApi['api'].request).toBeCalledWith({
      method: 'GET',
      url: expect.stringContaining(`?${params.toString()}`),
      headers: {
        Authorization: `Bearer ${token}`,
      },
    })
  })
})
