import { LOCALES } from 'src/app/constants'

describe('Locales.constants', () => {
  it('should all locale keys be uppercase', () => {
    const allLocaleKeysAreValid = Object.keys(LOCALES).every((key) => {
      const isKeyString = typeof key === 'string'
      const isKeyUppercase = key === key.toUpperCase()
      return isKeyString && isKeyUppercase
    })

    expect(allLocaleKeysAreValid).toBe(true)
  })

  it('should all locale values be xx or xx-XX', () => {
    const allLocaleValuesAreValid = Object.values(LOCALES).every((value) => {
      const isValueString = typeof value === 'string'
      const isValueLowercase = value === value.toLowerCase()

      if (value.includes('-')) {
        const [lang, tag] = value.split('-')
        const isLangLowercase = lang === lang.toLowerCase()
        const isTagUppercase = tag === tag.toUpperCase()
        return isValueString && isLangLowercase && isTagUppercase
      }

      return isValueString && isValueLowercase
    })

    expect(allLocaleValuesAreValid).toBe(true)
  })
})
