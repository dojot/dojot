import FormData from 'form-data'
import axios, { AxiosInstance } from 'axios'

import { Config } from 'src/types'

type TokenMetadata = {
  token: string
}

type UploadFileData = {
  file: Buffer
  path: string
}

type UploadFileReturn = {
  message: string
  details: {
    transactionCode: string
    filename: string
    info: {
      etag: string
      versionId: string
    }
    encoding: string
    mimetype: string
  }
}

type RemoveFileReturn = {
  message: string
  details: {
    size: number
    metadata: {
      'content-type': string
    }
    lastModified: string
    versionId: string
    etag: string
  }
}

export class FileManagementApi {
  private api: AxiosInstance

  constructor(private config: Config) {
    this.api = axios.create({ baseURL: `${this.config.apis.filemgmt}/api/v1` })
  }

  private getBearerToken(token: string): string {
    if (token.includes('Bearer')) return token
    return `Bearer ${token}`
  }

  async upload(data: UploadFileData, metadata: TokenMetadata) {
    const formData = new FormData()
    formData.append('file', data.file)
    formData.append('path', data.path)

    const response = await this.api.put<UploadFileReturn>(
      '/files/upload',
      formData,
      {
        headers: {
          Authorization: this.getBearerToken(metadata.token),
        },
      },
    )

    return response.data
  }

  async delete(path: string, metadata: TokenMetadata) {
    const params = new URLSearchParams({ path })

    const response = await this.api.delete<RemoveFileReturn>(
      `/files/remove?${params.toString()}`,
      {
        headers: {
          Authorization: this.getBearerToken(metadata.token),
        },
      },
    )

    return response.data
  }
}
