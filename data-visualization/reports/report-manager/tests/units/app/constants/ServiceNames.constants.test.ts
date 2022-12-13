import { SERVICE_NAMES } from 'src/app/constants'

describe('ServiceNames.constants', () => {
  it('should all service names have an uppercase key with lowercase value', () => {
    const allServiceNamesAreValid = Object.entries(SERVICE_NAMES).every(
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

    expect(allServiceNamesAreValid).toBe(true)
  })
})
