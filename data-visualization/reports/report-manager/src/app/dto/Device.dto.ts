import { REPORT_FORMATS } from 'src/app/constants'

export interface DevicesParamsDto {
  id: string
  label: string
  attrs: Array<{
    id: number
    label: string
    type: string
    valueType: string
  }>
}

export interface CreateDeviceReportDto {
  name: string
  format: REPORT_FORMATS.PDF | REPORT_FORMATS.CSV
  singleReportFile?: boolean
  initialDate?: string
  finalDate?: string
  devices: Array<DevicesParamsDto>
}
