
module.exports = function parseDojotTableMeta(tableMeta) {
  const identity = (x) => x;

  const typeSerializers = {
    boolean: identity,
    long(x) { return (x === '' ? null : +x); },
    double(x) {
      switch (x) {
        case '':
          return null;
        case '+Inf':
          return Number.POSITIVE_INFINITY;
        case '-Inf':
          return Number.NEGATIVE_INFINITY;
        default:
          return +x;
      }
    },
    string: identity,
    base64Binary: identity,
    duration(x) { return (x === '' ? null : x); },
    'dateTime:RFC3339': function (x) { return (x === '' ? null : x); },
  };

  const get = function (row) {
    // eslint-disable-next-line no-underscore-dangle
    let _a;
    let val = row[this.index];
    if ((val === '' || val === undefined) && this.defaultValue) {
      val = this.defaultValue;
    }
    // eslint-disable-next-line no-return-assign, no-cond-assign, no-void
    return ((_a = typeSerializers[this.dataType]) !== null && _a !== void 0 ? _a : identity)(val);
  };

  // eslint-disable-next-line no-param-reassign
  tableMeta.toObject = function (row) {
    const acc = {};
    // eslint-disable-next-line no-plusplus
    for (let i = 0; i < this.columns.length && i < row.length; i++) {
      const column = this.columns[i];
      column.get = get;
      acc[column.label] = column.get(row);
    }
    return acc;
  };

  return tableMeta;
};
