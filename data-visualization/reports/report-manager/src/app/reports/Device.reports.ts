import pdfmake from 'pdfmake'
import { Logger } from '@dojot/microservice-sdk'
import { createObjectCsvStringifier } from 'csv-writer'
import type { TableCell, TDocumentDefinitions } from 'pdfmake/interfaces'

import { LocaleManager } from 'src/app/locales'

interface DataForCsvReport {
  timestamp: string
  deviceId: string
  deviceLabel: string
  attr: string
  value: string | number
}

interface DataForPdfReport {
  deviceId: string
  deviceLabel: string
  attrs: Array<{
    timestamp: string
    label: string
    value: string | number
  }>
}

export class DeviceReports {
  constructor(private logger: Logger, private localeManager: LocaleManager) {
    this.logger.debug('constructor: device reports instantiated', {})
  }

  private getText(key: string) {
    return this.localeManager.get('DeviceReports', key)
  }

  setLocale(lang: string) {
    this.localeManager.setLocale(lang)
  }

  createCsv(data: DataForCsvReport[]): Buffer {
    this.logger.debug('createCsv: creating CSV report of devices', {})

    const csvWriter = createObjectCsvStringifier({
      header: [
        { id: 'timestamp', title: this.getText('csv.timestamp') },
        { id: 'deviceId', title: this.getText('csv.deviceId') },
        { id: 'deviceLabel', title: this.getText('csv.deviceLabel') },
        { id: 'attr', title: this.getText('csv.attribute') },
        { id: 'value', title: this.getText('csv.value') },
      ],
    })

    const header = csvWriter.getHeaderString() || ''
    const content = csvWriter.stringifyRecords(data)
    const csv = header + content
    return Buffer.from(csv, 'utf-8')
  }

  async createPdf(data: DataForPdfReport[]): Promise<Buffer> {
    return new Promise((resolve, reject) => {
      this.logger.debug('createPdf: creating PDF report of devices', {})

      const content: TDocumentDefinitions['content'] = []

      data.forEach(({ deviceId, deviceLabel, attrs }) => {
        content.push({
          text: `${deviceLabel} - ${deviceId}`,
          bold: true,
          fontSize: 16,
          margin: [0, 0, 0, 8],
        })

        const body: [TableCell, TableCell, TableCell][] = [
          [
            this.getText('pdf.attribute'),
            this.getText('pdf.value'),
            this.getText('pdf.date'),
          ],
        ]

        attrs.forEach(({ timestamp, label, value }) => {
          const locale = this.localeManager.getLocale()

          body.push([
            label,
            String(value),
            new Date(timestamp).toLocaleString(locale),
          ])
        })

        content.push({
          layout: 'headerLineOnly',
          margin: [0, 0, 0, 16],
          table: {
            headerRows: 1,
            widths: ['*', '*', '*'],
            body,
          },
        })
      })

      const definition: TDocumentDefinitions = {
        defaultStyle: { font: 'Helvetica' },
        content,
      }

      const printer = new pdfmake({
        // Standard fonts list:
        // https://pdfmake.github.io/docs/0.1/fonts/standard-14-fonts
        Helvetica: {
          normal: 'Helvetica',
          bold: 'Helvetica-Bold',
          italics: 'Helvetica-Oblique',
          bolditalics: 'Helvetica-BoldOblique',
        },
      })

      const pdf = printer.createPdfKitDocument(definition)

      const chunks: Uint8Array[] = []
      pdf.on('data', (chunk) => chunks.push(chunk))

      pdf.on('end', () => {
        const result = Buffer.concat(chunks)
        resolve(result)
      })

      pdf.on('error', reject)

      pdf.end()
    })
  }
}
