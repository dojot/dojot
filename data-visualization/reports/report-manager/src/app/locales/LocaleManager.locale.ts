import { Logger } from '@dojot/microservice-sdk'

import { LOCALES } from 'src/app/constants'

import * as PT_BR from './PT_BR'
import * as EN_US from './EN_US'

type Translations = { [key: string]: string | object }
type Namespaces = { [key: string]: Translations }

export class LocaleManager {
  private locale: string | undefined

  constructor(private logger: Logger) {
    this.logger.debug(`constructor: locales = ${Object.values(LOCALES)}`, {})
    this.logger.debug(`constructor: default locale = ${this.locale}`, {})
  }

  private getNamespaces(): Namespaces {
    switch (this.locale) {
      case LOCALES.PT_BR:
        return PT_BR
      case LOCALES.EN_US:
        return EN_US
      default:
        return {}
    }
  }

  getLocale() {
    return this.locale
  }

  setLocale(locale: string) {
    this.locale = locale
    this.logger.debug(`setLocale: ${this.locale}`, {})
    return this
  }

  get(namespace: string, key: string): string {
    if (!namespace || !key) return ''
    const namespaces = this.getNamespaces()
    const translations = namespaces[namespace]
    if (!translations) return key
    const value = translations[key]
    if (typeof value === 'string') return value || ''
    return key
  }
}
