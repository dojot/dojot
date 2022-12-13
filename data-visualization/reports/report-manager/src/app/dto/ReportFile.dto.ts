export interface CreateReportFileDto {
  reportId: string
  path: string
  mimeType: string
  filename: string
  fileSizeKb: number
  expiresAt?: string | null
}
