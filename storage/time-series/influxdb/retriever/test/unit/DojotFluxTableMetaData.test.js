const {
  createFluxTableColumn,
  createFluxTableMetaData,
} = require('@influxdata/influxdb-client');
const parseDojotTableMeta = require('../../app/influx/DojotFluxTableMetaData');

describe('DojotFluxTableMetadata', () => {
  let dojotTableMeta;

  // Init a default FluxTableMetaData
  beforeAll(() => {
    const columns = [
      createFluxTableColumn({
        label: 'test_boolean',
        dataType: 'boolean',
        defaultValue: null,
        group: true,
        index: 1,
      }),
      createFluxTableColumn({
        label: 'test_long',
        dataType: 'long',
        defaultValue: null,
        group: true,
        index: 1,
      }),
      createFluxTableColumn({
        label: 'test_double',
        dataType: 'double',
        defaultValue: null,
        group: true,
        index: 1,
      }),
    ];
    const tableMeta = createFluxTableMetaData(columns);

    dojotTableMeta = parseDojotTableMeta(tableMeta);
  });

  it('Happy flow', () => {
    const object = dojotTableMeta.toObject([true, 1, 1.2]);

    expect(object.test_boolean).toBeTruthy();
    expect(object.test_long).toEqual(1);
    expect(object.test_double).toEqual(1.2);
  });

  it('Should return null when the boolean field is null', () => {
    const object = dojotTableMeta.toObject([null]);

    expect(object.test_boolean).toBeNull();
  });

  it('Should return "false" in boolean field', () => {
    const object = dojotTableMeta.toObject([false]);

    expect(object.test_boolean).toBeFalsy();
  });

  it('Should return "POSITIVE_INFINITY" in double field', () => {
    const object = dojotTableMeta.toObject([true, 1, '+Inf']);

    expect(object.test_double).toEqual(Number.POSITIVE_INFINITY);
  });

  it('Should return "NEGATIVE_INFINITY" in double field', () => {
    const object = dojotTableMeta.toObject([true, 1, '-Inf']);

    expect(object.test_double).toEqual(Number.NEGATIVE_INFINITY);
  });
});
