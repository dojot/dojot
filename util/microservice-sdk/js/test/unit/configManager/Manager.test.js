const Manager = require('../../../lib/configManager/Manager');
const Parsers = require('../../../lib/configManager/parsers');
const FileManager = require('../../../lib/configManager/fileManager');
const Merger = require('../../../lib/configManager/Merger');

jest.mock('../../../lib/configManager/parsers', () => ({
  EnvVars: {
    parseEnvironmentVariables: jest.fn(),
  },
}));
jest.mock('../../../lib/configManager/fileManager', () => ({
  Reader: {
    readDefaultConfig: jest.fn(),
    readJson: jest.fn(),
    readUserConfig: jest.fn(),
  },
  Writer: {
    writeJson: jest.fn(),
  },
}));
jest.mock('../../../lib/configManager/Merger', () => ({
  mergeConfigs: jest.fn(),
}));

describe('loadSettings', () => {
  it('should successfully create the configuration', () => {
    Parsers.EnvVars.parseEnvironmentVariables.mockReturnValueOnce(['testParam1=10']);
    FileManager.Reader.readUserConfig.mockReturnValueOnce(['testParam2=20']);
    FileManager.Reader.readDefaultConfig.mockReturnValueOnce(['testParam3=30']);
    Merger.mergeConfigs.mockReturnValueOnce({
      testParam1: '10',
      testParam2: '20',
      testParam3: '30',
    });

    Manager.loadSettings('TESTSVC');

    expect(Parsers.EnvVars.parseEnvironmentVariables).toHaveBeenCalledTimes(1);
    expect(FileManager.Reader.readUserConfig).toHaveBeenCalledTimes(1);
    expect(FileManager.Reader.readDefaultConfig).toHaveBeenCalledTimes(1);
    expect(Merger.mergeConfigs).toHaveBeenCalledTimes(1);
    expect(FileManager.Writer.writeJson).toHaveBeenCalledTimes(1);

    expect(Parsers.EnvVars.parseEnvironmentVariables.mock.results.type).not.toEqual('throw');
    expect(FileManager.Reader.readUserConfig.mock.results.type).not.toEqual('throw');
    expect(FileManager.Reader.readDefaultConfig.mock.results.type).not.toEqual('throw');
    expect(Merger.mergeConfigs.mock.results.type).not.toEqual('throw');
    expect(FileManager.Writer.writeJson.mock.results.type).not.toEqual('throw');
  });
});

describe('getConfig', () => {
  it('should successfully retrieve the configuration', () => {
    const data = { paramTest: 10 };
    FileManager.Reader.readJson.mockImplementationOnce(() => data);

    const config = Manager.getConfig('TESTSVC');

    expect(config).toEqual(data);
  });
});
