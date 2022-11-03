import axios, { AxiosInstance } from 'axios'

import { Config } from 'src/types'

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
  private api: AxiosInstance

  constructor(private config: Config) {
    this.api = axios.create({ baseURL: `${this.config.apis.retriever}/tss/v1` })
  }

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

    const response = await this.api.get<GetDeviceDataReturn>(
      `/devices/${deviceId}/data?${params.toString()}`,
      {
        headers: {
          Authorization: `Bearer ${metadata.token}`,
        },
      },
    )

    return response.data
  }
}
