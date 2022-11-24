import { LoggerMock } from 'tests/mocks'
import { LOCALES } from 'src/app/constants'
import { LocaleManager } from 'src/app/locales'

describe('LocaleManager.locale', () => {
  it('should set the locale', () => {
    const localeManager = new LocaleManager(LoggerMock.new())
    expect(localeManager.getLocale()).toBeUndefined()
    localeManager.setLocale(LOCALES.PT_BR)
    expect(localeManager.getLocale()).toBe(LOCALES.PT_BR)
    localeManager.setLocale(LOCALES.EN_US)
    expect(localeManager.getLocale()).toBe(LOCALES.EN_US)
  })

  it('should return empty string if the namespace or key are falsy', () => {
    const localeManager = new LocaleManager(LoggerMock.new())
    localeManager.setLocale(LOCALES.EN_US)
    const falsyValues = [undefined, null, 0, '', NaN]
    falsyValues.forEach((falsyValue) => {
      expect(localeManager.get(falsyValue as never, 'key')).toBe('')
      expect(localeManager.get('namespace', falsyValue as never)).toBe('')
    })
  })

  it('should return the key if does not set a locale', () => {
    const localeManager = new LocaleManager(LoggerMock.new())
    expect(localeManager.getLocale()).toBeUndefined()
    const translation = localeManager.get('namespace', 'key')
    expect(translation).toBe('key')
  })

  it('should return the key if the namespace does not exist', () => {
    const localeManager = new LocaleManager(LoggerMock.new())
    localeManager.setLocale(LOCALES.EN_US)
    const namespace = '__DOES_NOT_EXIST__'
    const translation = localeManager.get(namespace, 'key')
    expect(translation).toBe('key')
  })

  it('should return the key if the namespace exists but the key not', () => {
    const localeManager = new LocaleManager(LoggerMock.new())
    localeManager.setLocale(LOCALES.EN_US)
    const namespaces = localeManager['getNamespaces']()

    const [firstNamespace] = Object.keys(namespaces)
    expect(firstNamespace).toBeDefined()

    const key = '__DOES_NOT_EXIST__'
    const translation = localeManager.get(firstNamespace, key)
    expect(translation).toBe(key)
  })

  it('should return one translation for all locales', () => {
    const localeManager = new LocaleManager(LoggerMock.new())
    Object.values(LOCALES).forEach((locale) => {
      localeManager.setLocale(locale)
      const namespaces = localeManager['getNamespaces']()

      const [firstNamespace] = Object.keys(namespaces)
      expect(firstNamespace).toBeDefined()

      const [firstKey] = Object.keys(namespaces[firstNamespace])
      expect(firstKey).toBeDefined()

      const translation = localeManager.get(firstNamespace, firstKey)
      expect(translation).not.toBeFalsy()
      expect(translation).not.toBe(firstKey)
      expect(translation).toBe(namespaces[firstNamespace][firstKey])
    })
  })
})
