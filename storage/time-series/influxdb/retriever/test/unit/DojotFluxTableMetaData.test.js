const {
  createFluxTableColumn,
  createFluxTableMetaData
} = require('@influxdata/influxdb-client');
const parseDojotTableMeta = require('../../app/influx/DojotFluxTableMetaData');

describe('DojotFluxTableMetadata', () => {
  let dojotTableMeta;

  // Init a default FluxTableMetaData 
  beforeAll(() => {
    const columns = [
      createFluxTableColumn({
        label: 'test',
        dataType: 'boolean',
        defaultValue: null,
        group: true,
        index: 1,
        get: (x) => true,
      })
    ];
    const tableMeta = createFluxTableMetaData(columns);

    dojotTableMeta = parseDojotTableMeta(tableMeta);
  });

  it('Should return null when the boolean field is null', () => {
    const object = dojotTableMeta.toObject([null]);

    expect(object['test']).toBeNull();
  });

  it('Should return "true"', () => {
    const object = dojotTableMeta.toObject([true]);

    expect(object['test']).toBeTruthy();
  });

  it('Should return "true"', () => {
    const object = dojotTableMeta.toObject([false]);

    expect(object['test']).toBeFalsy();
  });
});