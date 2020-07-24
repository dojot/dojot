const fs = require('fs');

const jsonStringify = require('fast-safe-stringify');

const Writer = require('../../../../lib/ConfigManager/FileManager/Writer');
const Utils = require('../../../../lib/ConfigManager/Utils');

jest.mock('fs');

jest.mock('../../../../lib/ConfigManager/Utils', () => ({
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
