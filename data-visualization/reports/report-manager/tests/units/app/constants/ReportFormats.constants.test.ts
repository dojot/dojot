import { REPORT_FORMATS } from 'src/app/constants'

describe('ReportFormats.constants', () => {
  it('should all report formats have an uppercase key with lowercase value', () => {
    const allReportFormatsAreValid = Object.entries(REPORT_FORMATS).every(
      ([key, value]) => {
        const isKeyString = typeof key === 'string'
        const isKeyUppercase = key === key.toUpperCase()
        const isValueString = typeof value === 'string'
        const isValueLowercase = value === value.toLowerCase()

        return (
          isKeyString && isKeyUppercase && isValueString && isValueLowercase
        )
      },
    )

    expect(allReportFormatsAreValid).toBe(true)
  })
})
