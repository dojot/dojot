const fs = require('fs');

const jsonStringify = require('fast-safe-stringify');

const Writer = require('../../../../lib/configManager/fileManager/Writer');
const Utils = require('../../../../lib/configManager/Utils');

jest.mock('fs');

jest.mock('../../../../lib/configManager/Utils', () => ({
  createFilename: jest.fn(() => './config/testfile.json'),
}));

describe('writeJson', () => {
  it('should write the file', () => {
    const data = { testParam: 10 };
    const dataInJson = jsonStringify(data);

    Writer.writeJson('testfile', './config', data);

    expect(Utils.createFilename).toHaveBeenCalledWith('testfile.json', './config');
    expect(fs.writeFileSync).toHaveBeenCalledWith('./config/testfile.json', dataInJson);
  });
});
