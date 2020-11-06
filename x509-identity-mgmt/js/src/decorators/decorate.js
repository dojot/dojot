/**
 * A decoration is nothing more than an isolated piece of code applicable to one
 * or more functions or class methods. The logic to be applied depends on the
 * problem to be solved, but the fundamental point is the centralization of this
 * logic, thus avoiding duplication of code.
 * @param {Object} target where decorators will be kept.
 * @param {Array} methods that will be decorated.
 * @param {Array} decorators decorators with the logic to be applied on the original methods.
 */
function decorate(target, methods, decorators) {
  // The list must be inverted so that decorators are executed
  // in the correct  order when the decorated method is called
  decorators.reverse();

  //
  methods.forEach((methodName) => {
    decorators.forEach((decorator) => {
      const method = target[methodName].bind(target);

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
      }

      if (interceptor) {
        Reflect.defineProperty(target, methodName, {
          configurable: true,
          writable: false,
          enumerable: false,
          value: interceptor,
        });
      }
    });
  });
}

module.exports = decorate;
