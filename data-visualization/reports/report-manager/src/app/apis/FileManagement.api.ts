import FormData from 'form-data'
import { WebUtils } from '@dojot/microservice-sdk'

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
  constructor(private api: WebUtils.DojotHttpClient) {}

  private getBearerToken(token: string): string {
    if (token.includes('Bearer')) return token
    return `Bearer ${token}`
  }

  async upload(data: UploadFileData, metadata: TokenMetadata) {
    const formData = new FormData()
    formData.append('file', data.file)
    formData.append('path', data.path)

    const response: { data: UploadFileReturn } = await this.api.request({
      method: 'PUT',
      url: '/files/upload',
      data: formData,
      headers: formData.getHeaders({
        Authorization: this.getBearerToken(metadata.token),
      }),
    })

    return response.data
  }

  async delete(path: string, metadata: TokenMetadata) {
    const params = new URLSearchParams({ path })

    const response: { data: RemoveFileReturn } = await this.api.request({
      method: 'DELETE',
      url: `/files/remove?${params.toString()}`,
      headers: {
        Authorization: this.getBearerToken(metadata.token),
      },
    })

    return response.data
  }
}
