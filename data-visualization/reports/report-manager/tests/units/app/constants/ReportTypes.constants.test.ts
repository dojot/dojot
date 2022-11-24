import { REPORT_TYPES } from 'src/app/constants'

describe('ReportTypes.constants', () => {
  it('should all report type keys and values be uppercase strings', () => {
    const allReportTypesAreValid = Object.entries(REPORT_TYPES).every(
      ([key, value]) => {
        const isKeyString = typeof key === 'string'
        const isKeyUppercase = key === key.toUpperCase()
        const isValueString = typeof value === 'string'
        const isValueUppercase = value === value.toUpperCase()

        return (
          isKeyString && isKeyUppercase && isValueString && isValueUppercase
        )
      },
    )

    expect(allReportTypesAreValid).toBe(true)
  })
})
