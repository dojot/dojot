module.exports = ({ levelDebug }) => {
  /**
   * A decoration is nothing more than an isolated piece of code applicable to one
   * or more functions or class methods. The logic to be applied depends on the
   * problem to be solved, but the fundamental point is the centralization of this
   * logic, thus avoiding duplication of code.
   * @param {Object} target where decorators will be kept.
   * @param {Array} methods that will be decorated.
   * @param {Array} decorators decorators with the logic to be applied on the original methods.
   */
  function decorate(
    target, methods, decorators,
  ) {
    // For the order of execution of the decorators to follow the same order
    // in which they were declared in the list, it is necessary to apply the
    // decoration starting with the last decorator of the list and thus until
    // the first...
    // Instead of going through the list from the end to the beginning, we can
    // invert it and then iterate through it with the "forEach()" method.
    decorators.reverse();

    methods.forEach((methodName) => {
      decorators.forEach((decorator) => {
        const method = Reflect.get(target, methodName).bind(target);

        /* detect if method is asynchronous (async/await) */
        const isAsyncMethod = method.constructor.name === 'AsyncFunction';
        const isAsyncDecorator = decorator.decorate.constructor.name === 'AsyncFunction';

        let interceptor = null;

        if (isAsyncMethod && isAsyncDecorator) {
          interceptor = async (...args) => (
            decorator.decorate({
              target, method, args, methodName,
            })
          );
        } else if (!isAsyncMethod && !isAsyncDecorator) {
          interceptor = (...args) => (
            decorator.decorate({
              target, method, args, methodName,
            })
          );
        } else {
          throw new Error(`The execution mode of the method (${methodName}) and the decorator`
            + ' are divergent, one is synchronous and the other is asynchronous!');
        }

        Reflect.defineProperty(
          target, methodName, {
            configurable: true,
            writable: false,
            enumerable: false,
            value: interceptor,
          },
        );
      });
    });
  }

  const decorateToDebug = (
    target, methods, asyncMethods, containerCradle,
  ) => {
    if (levelDebug === true || (typeof levelDebug === 'function' && levelDebug())) {
      // Decorates synchronous methods
      if (methods.length) {
        const decorators = [
          containerCradle.logExecutionTimeDecorator,
          containerCradle.inspectMethodDecorator,
        ];
        decorate(
          target, methods, decorators,
        );
      }

      // Decorate asynchronous methods
      if (asyncMethods.length) {
        const asyncDecorators = [
          containerCradle.logExecutionTimeAsyncDecorator,
          containerCradle.inspectMethodAsyncDecorator,
        ];
        decorate(
          target, asyncMethods, asyncDecorators,
        );
      }
    }
  };

  const fromDecoratedClass = (clazz) => (containerCradle) => {
    const instance = Reflect.construct(clazz, [containerCradle]);

    const methods = Reflect.ownKeys(clazz.prototype).filter(((key) => key !== 'constructor'
      && typeof Reflect.get(clazz.prototype, key) === 'function'
      && Reflect.get(clazz.prototype, key).constructor.name !== 'AsyncFunction'));

    const asyncMethods = Reflect.ownKeys(clazz.prototype).filter(((key) => key !== 'constructor'
      && typeof Reflect.get(clazz.prototype, key) === 'function'
      && Reflect.get(clazz.prototype, key).constructor.name === 'AsyncFunction'));

    decorateToDebug(
      instance, methods, asyncMethods, containerCradle,
    );
    return instance;
  };

  const fromDecoratedFactory = (factory) => (containerCradle) => {
    const instance = factory(containerCradle);

    const methods = Reflect.ownKeys(instance).filter(((key) => typeof Reflect.get(instance, key) === 'function'
      && Reflect.get(instance, key).constructor.name !== 'AsyncFunction'));

    const asyncMethods = Reflect.ownKeys(instance).filter(((key) => typeof Reflect.get(instance, key) === 'function'
      && Reflect.get(instance, key).constructor.name === 'AsyncFunction'));

    decorateToDebug(
      instance, methods, asyncMethods, containerCradle,
    );
    return instance;
  };

  return {
    decorate, fromDecoratedClass, fromDecoratedFactory,
  };
};
