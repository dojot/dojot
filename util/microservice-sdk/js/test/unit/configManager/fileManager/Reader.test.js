const fs = require('fs');

const Reader = require('../../../../lib/configManager/fileManager/Reader');
const Sanitizer = require('../../../../lib/configManager/fileManager/Sanitizer');
const Utils = require('../../../../lib/configManager/Utils');

jest.mock('fs');

jest.mock('../../../../lib/configManager/Utils', () => ({
  createFilename: jest.fn(() => './config/testfile'),
}));
jest.mock('../../../../lib/logging/Logger', () => ({
  Logger: jest.fn(() => ({
    debug: jest.fn(),
  })),
}));
jest.mock('../../../../lib/configManager/fileManager/Sanitizer', () => ({
  sanitize: jest.fn((data) => data),
}));

describe('readDefaultConfig', () => {
  it('should successfully read the default configuration file', () => {
    const data = 'testData';
    fs.existsSync.mockReturnValueOnce(true);
    fs.readFileSync.mockReturnValueOnce(data);

    const ret = Reader.readDefaultConfig();

    expect(ret).toEqual(data);
    expect(Sanitizer.sanitize).toHaveBeenCalledTimes(1);
  });

  it('should throw an error when reading the default configuration file - empty file', () => {
    fs.existsSync.mockReturnValueOnce(true);
    fs.readFileSync.mockReturnValueOnce('');

    expect(() => Reader.readDefaultConfig()).toThrow();
    expect(Sanitizer.sanitize).not.toHaveBeenCalled();
  });

  it('should throw an error when reading the default configuration file - no such file', () => {
    fs.existsSync.mockReturnValueOnce(false);

    expect(() => Reader.readDefaultConfig()).toThrow();
    expect(Sanitizer.sanitize).not.toHaveBeenCalled();
  });
});


describe('readUserConfig', () => {
  it('should successfully read the user configuration file', () => {
    const data = 'testData';
    fs.existsSync.mockReturnValueOnce(true);
    fs.readFileSync.mockReturnValueOnce(data);

    const ret = Reader.readUserConfig();

    expect(ret).toEqual(data);
    expect(Utils.createFilename).toHaveBeenCalledTimes(1);
  });

  it('should not throw an error when the user configuration file does not exist', () => {
    fs.existsSync.mockReturnValueOnce(false);

    const ret = Reader.readUserConfig();

    expect(ret).toEqual([]);
    expect(Utils.createFilename).toHaveBeenCalledTimes(1);
  });
});

describe('readJson', () => {
  it('should successfully read the JSON configuration file', () => {
    const data = '{"testParam":10}';
    const dataObj = JSON.parse(data);
    fs.existsSync.mockReturnValueOnce(true);
    fs.readFileSync.mockReturnValueOnce(data);

    const ret = Reader.readJson();

    expect(ret).toEqual(dataObj);
    expect(Utils.createFilename).toHaveBeenCalledTimes(1);
  });

  it('should not throw an error when the JSON configuration file does not exist', () => {
    fs.existsSync.mockReturnValueOnce(false);

    const ret = Reader.readJson();

    expect(ret).toEqual({});
    expect(Utils.createFilename).toHaveBeenCalledTimes(1);
  });
});
