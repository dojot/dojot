class CommonModel {
  constructor({
    mongoClient, mongooseModel, mongoQS, projectableFields, sortByFields,
  }) {
    Object.defineProperty(this, 'mongoQS', { value: mongoQS });
    Object.defineProperty(this, 'mongoClient', { value: mongoClient });
    Object.defineProperty(this, 'model', { value: mongooseModel });
    Object.defineProperty(this, 'projectableFields', { value: projectableFields });
    Object.defineProperty(this, 'sortByFields', { value: sortByFields });
  }

  parseConditionFields(candidates) {
    const conditionFields = { ...candidates };
    Object.entries(conditionFields).forEach(
      ([key, value]) => {
        // value must be a string
        if (typeof value !== 'string') {
          if (typeof value.toString !== 'function') {
            throw new Error('The value of the Condition Field must be a string or convertible to a string.');
          }
          Reflect.set(conditionFields, key, value.toString());
        }
      },
    );
    return this.mongoQS.parse(conditionFields);
  }

  parseProjectionFields(commaSeparatedFields) {
    return this.mongoClient.parseProjectionFields(commaSeparatedFields, this.projectableFields);
  }

  sanitizeFields(cert) {
    return this.mongoClient.sanitizeFields(cert, this.projectableFields);
  }

  handleFilterField(candidates) {
    if (!candidates) {
      return {};
    }

    const filterField = candidates.split(',').reduce((e, i) => {
      const keyVal = i.split(/[=:|]/);
      e[keyVal[0]] = keyVal[1] === 'null' ? '!' : keyVal[1];
      return e;
    }, {});

    return this.parseConditionFields(filterField);
  }

  parseSortBy(sortBy) {
    if (typeof sortBy !== 'string') return null;
    if (!this.sortByFields || this.sortByFields.length === 0) return null;

    const isSortByValid = this.sortByFields.includes(sortBy);
    if (!isSortByValid) return null;
    
    if (sortBy.includes(':')) {
      const [order, field] = sortBy.split(':');
      return { [field]: order };
    }

    return { [sortBy]: 'asc' };
  }
}

module.exports = CommonModel;
