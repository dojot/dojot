
class LogExecutionTime {
  constructor({ logger }) {
    Object.defineProperty(this, 'logger', { value: logger });
  }

  decorate({
    target, method, args, methodName,
  }) {
    const start = new Date();
    const result = method.apply(target, args);
    const end = new Date() - start;
    this.logger.debug(`${target.constructor.name}.${methodName} - Execution time: ${end}ms`);
    return result;
  }
}

module.exports = LogExecutionTime;
