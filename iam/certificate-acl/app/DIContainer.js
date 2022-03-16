const awilix = require('awilix');

const { Logger } = require('@dojot/microservice-sdk');

const {
  asClass, Lifetime, InjectionMode,
} = awilix;

function createObject() {
  const DIContainer = awilix.createContainer();

  const modules = {
    logger: asClass(Logger, {
      injectionMode: InjectionMode.CLASSIC,
      injector: () => ({ sid: 'Certificate-ACL - Main' }),
      lifetime: Lifetime.SINGLETON,
    }),
  };

  DIContainer.register(modules);
  return DIContainer;
}

module.exports = () => createObject();
