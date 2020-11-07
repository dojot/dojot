
class LogExecutionTimeAsync {
  constructor({ logger }) {
    Object.defineProperty(this, 'logger', { value: logger });
  }

  async decorate({
    target, method, args, methodName,
  }) {
    const start = new Date();
    const result = await method.apply(target, args);
    const end = new Date() - start;
    this.logger.debug(`${target.constructor.name}.${methodName} - Execution time: ${end}ms`);
    return result;
  }
}

module.exports = LogExecutionTimeAsync;
