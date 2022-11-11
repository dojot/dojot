import { WebUtils } from '@dojot/microservice-sdk'

type GetDeviceDataFilters = {
  dateFrom?: string
  dateTo?: string
  limit?: number
  page?: number
  order?: 'asc' | 'desc'
}

type GetDeviceDataMetadata = {
  token: string
}

type PaginationObject = {
  number: number
  url: string
}

type GetDeviceDataReturn = {
  data: Array<{
    ts: string
    attrs: Array<{
      label: string
      value: string
    }>
  }>
  paging: {
    current: PaginationObject
    next: PaginationObject | null
    previous: PaginationObject | null
  }
}

export class RetrieverApi {
  constructor(private api: WebUtils.DojotHttpClient) {}

  async getDeviceData(
    deviceId: string,
    metadata: GetDeviceDataMetadata,
    filters?: GetDeviceDataFilters,
  ) {
    const params = new URLSearchParams()
    if (filters?.dateFrom) params.append('dateFrom', filters.dateFrom)
    if (filters?.dateTo) params.append('dateTo', filters.dateTo)
    if (filters?.limit) params.append('limit', String(filters.limit))
    if (filters?.page) params.append('page', String(filters.page))
    if (filters?.order) params.append('order', filters.order)

    const response: { data: GetDeviceDataReturn } = await this.api.request({
      method: 'GET',
      url: `/devices/${deviceId}/data?${params.toString()}`,
      headers: {
        Authorization: `Bearer ${metadata.token}`,
      },
    })

    return response.data
  }
}
