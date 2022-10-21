const awilix = require('awilix');

const { Logger } = require('@dojot/microservice-sdk');

const {
  asClass, asFunction, Lifetime, InjectionMode,
} = awilix;

const scopedDIInterceptor = require('./server/express/interceptors/scopedDIInterceptor');

function createObject() {
  const DIContainer = awilix.createContainer();

  const modules = {
    logger: asClass(Logger, {
      injectionMode: InjectionMode.CLASSIC,
      injector: () => ({ sid: 'certificate-acl' }),
      lifetime: Lifetime.SINGLETON,
    }),
    scopedDIInterceptor: asFunction(scopedDIInterceptor, {
      injector: () => ({ DIContainer, path: '/' }),
      lifetime: Lifetime.SINGLETON,
    }),
  };

  DIContainer.register(modules);
  return DIContainer;
}

module.exports = () => createObject();
