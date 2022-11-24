export interface CreateReportAttemptDto {
  reportId: string
}

export interface UpdateReportAttemptDto {
  error?: string
  failedAt?: string
  canceledAt?: string
  finishedAt?: string
}
