import { HEADERS, LOCALES } from 'src/app/constants'
import { LoggerMock, ExpressMock } from 'tests/mocks'
import { LanguageInterceptor } from 'src/app/interceptors'

describe('Language.interceptor', () => {
  const DEFAULT_LANGUAGE = LOCALES.PT_BR

  const interceptor = LanguageInterceptor.use(LoggerMock.new())
  const [middleware] = interceptor.middleware

  it('should have the structure of an interceptor', () => {
    expect(interceptor.name).toBe(LanguageInterceptor.name)
    expect(Array.isArray(interceptor.path)).toBe(true)
    expect(Array.isArray(interceptor.middleware)).toBe(true)
  })

  it('should use the default language', () => {
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
    middleware(RequestMock, ResponseMock, NextFunctionMock)
    expect(RequestMock.lang).toBe(DEFAULT_LANGUAGE)
    expect(NextFunctionMock).toBeCalled()
  })

  it('should use the default language if language is not supported', () => {
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
    RequestMock.headers[HEADERS.LANG] = '_NOT_SUPPORTED_'
    middleware(RequestMock, ResponseMock, NextFunctionMock)
    expect(RequestMock.lang).toBe(DEFAULT_LANGUAGE)
    expect(NextFunctionMock).toBeCalled()
  })

  it('should use the default language if the lang header is invalid', () => {
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
    RequestMock.headers[HEADERS.LANG] = ['invalid']
    middleware(RequestMock, ResponseMock, NextFunctionMock)
    expect(RequestMock.lang).toBe(DEFAULT_LANGUAGE)
    expect(NextFunctionMock).toBeCalled()
  })

  it('should use another supported language', () => {
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
    RequestMock.headers[HEADERS.LANG] = LOCALES.EN_US
    middleware(RequestMock, ResponseMock, NextFunctionMock)
    expect(RequestMock.lang).toBe(LOCALES.EN_US)
    expect(NextFunctionMock).toBeCalled()
  })
})
