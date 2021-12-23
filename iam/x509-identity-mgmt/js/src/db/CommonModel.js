class CommonModel {
  constructor({
    mongoClient, mongooseModel, mongoQS, projectableFields,
  }) {
    Object.defineProperty(
      this, 'mongoQS', { value: mongoQS },
    );
    Object.defineProperty(
      this, 'mongoClient', { value: mongoClient },
    );
    Object.defineProperty(
      this, 'model', { value: mongooseModel },
    );
    Object.defineProperty(
      this, 'projectableFields', { value: projectableFields },
    );
  }

  parseConditionFields(candidates) {
    const conditionFields = { ...candidates };
    Object.entries(conditionFields).forEach(([key, value]) => {
      // value must be a string
      if (typeof value !== 'string') {
        if (typeof value.toString !== 'function') {
          throw new Error('The value of the Condition Field must be a string or convertible to a string.');
        }
        Reflect.set(
          conditionFields, key, value.toString(),
        );
      }
    });
    return this.mongoQS.parse(conditionFields);
  }

  parseProjectionFields(commaSeparatedFields) {
    return this.mongoClient.parseProjectionFields(commaSeparatedFields, this.projectableFields);
  }

  sanitizeFields(cert) {
    return this.mongoClient.sanitizeFields(cert, this.projectableFields);
  }
}

module.exports = CommonModel;
