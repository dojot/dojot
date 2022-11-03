import { Logger } from '@dojot/microservice-sdk'
import { NextFunction, Request, Response } from 'express'

import { DojotSdkInterceptor } from 'src/types'
import { HEADERS, LOCALES } from 'src/app/constants'

export abstract class LanguageInterceptor {
  static use(logger: Logger): DojotSdkInterceptor {
    const middleware = (req: Request, _: Response, next: NextFunction) => {
      const lang = req.headers[HEADERS.LANG]

      if (
        lang &&
        typeof lang === 'string' &&
        Object.values(LOCALES).includes(lang as LOCALES)
      ) {
        req.lang = lang
      } else {
        req.lang = LOCALES.PT_BR
      }

      logger.debug(`Language is set to: ${req.lang}`, {})
      return next()
    }

    return {
      path: ['/'],
      name: 'LanguageInterceptor',
      middleware: [middleware],
    }
  }
}
