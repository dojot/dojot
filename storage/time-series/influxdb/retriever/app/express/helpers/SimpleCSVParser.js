const json2csv = require('json2csv');

/**
 *
 * @param {[*]} attrData device attribute dataset
 * @returns device attribute dataset in CSV format
*/
function parseCSV(data) {
  const csvParser = new json2csv.Parser({ defaultValue: undefined });
  return data.length > 0 ? csvParser.parse(data) : '';
}

module.exports = {
  parseCSV,
};
