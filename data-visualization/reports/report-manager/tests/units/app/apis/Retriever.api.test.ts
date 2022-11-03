import { ConfigMock } from 'tests/mocks'
import { RetrieverApi } from 'src/app/apis'

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

jest.mock('axios', () => {
  return {
    create: jest.fn(() => ({
      get: jest.fn(() => ({
        data: FAKE_RETURNED_DATA,
      })),
    })),
  }
})

describe('Retriever.api', () => {
  it('should get device data with no filters', async () => {
    const retrieverApi = new RetrieverApi(ConfigMock.new())

    const token = '123456'
    const data = await retrieverApi.getDeviceData('id', { token })

    expect(data).toEqual(FAKE_RETURNED_DATA)
    expect(retrieverApi['api'].get).toBeCalledWith(expect.any(String), {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    })
  })

  it('should get device data using filters', async () => {
    const retrieverApi = new RetrieverApi(ConfigMock.new())

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
    expect(retrieverApi['api'].get).toBeCalledWith(
      expect.stringContaining(`?${params.toString()}`),
      {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      },
    )
  })
})
