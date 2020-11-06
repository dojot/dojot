class InspectMethod {
  constructor({ logger }) {
    Object.defineProperty(this, 'logger', { value: logger });
  }

  async decorate({
    target, method, args, methodName,
  }) {
    const name = `${target.constructor.name}.${methodName}`;
    this.logger.debug(`${name} - Arguments: `, args);
    const result = await method.apply(target, args);
    this.logger.debug(`${name} - Returns: `, result);
    return result;
  }
}

module.exports = InspectMethod;
