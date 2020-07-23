const transform = require('lodash/transform');

/**
 * Transforms all the keys in a object according to a transformation function.
 *
 * @param {*} obj
 * @param {(value: string) => string} mapFunction function to be applied to all keys in the object
 */
const transformObjectKeys = (obj, mapFunction) => {
  const newObj = transform(obj, (acc, value, key) => {
    acc[mapFunction(key)] = value;
    return acc;
  }, {});

  return newObj;
};

module.exports = { transformObjectKeys };
