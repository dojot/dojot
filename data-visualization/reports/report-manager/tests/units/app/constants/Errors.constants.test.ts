import { APP_ERRORS } from 'src/app/constants'

describe('Errors.constants', () => {
  it('should all app error keys and values be uppercase strings', () => {
    const allErrorsAreValid = Object.entries(APP_ERRORS).every(
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

    expect(allErrorsAreValid).toBe(true)
  })
})
