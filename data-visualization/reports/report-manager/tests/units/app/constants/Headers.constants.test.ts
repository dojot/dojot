import { HEADERS } from 'src/app/constants'

describe('Headers.constants', () => {
  it('should all header keys be uppercase and the values lowercase', () => {
    const allHeadersAreValid = Object.entries(HEADERS).every(([key, value]) => {
      const isKeyString = typeof key === 'string'
      const isKeyUppercase = key === key.toUpperCase()
      const isValueString = typeof value === 'string'
      const isValueLowercase = value === value.toLowerCase()
      return isKeyString && isKeyUppercase && isValueString && isValueLowercase
    })

    expect(allHeadersAreValid).toBe(true)
  })
})
