const MinIoRepositoryMock = require('../../mocks/minio-repository-mock');
const LoggerMock = require('../../mocks/logger-mock');

const mockValidator = {
  validate: jest.fn(),
};
jest.mock('../../../src/utils/path-validator-util', () => mockValidator);

const RetrievalFileService = require('../../../src/services/retrieval-file-service');

describe('RetrievalFileService', () => {
  let retrievalFileService;
  let minIoRepositoryMock;
  beforeEach(() => {
    minIoRepositoryMock = new MinIoRepositoryMock();
    minIoRepositoryMock.getObject = jest.fn();
    minIoRepositoryMock.getObjectUrl = jest.fn();
    retrievalFileService = new RetrievalFileService(minIoRepositoryMock, LoggerMock);
  });

  it('Should return the requeted file', async () => {
    minIoRepositoryMock.getObject.mockReturnValueOnce({});

    const file = await retrievalFileService.download('test', '/test/test_sample1');

    expect(file).toBeDefined();
  });

  it('Dowload > Should throw an error, when the file was not found', async () => {
    minIoRepositoryMock.getObject.mockReturnValueOnce(null);

    let error;
    try {
      await retrievalFileService.download('test', '/test/test_sample1');
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON).toEqual({
      error: 'The file does not exist.',
      detail: 'The file does not exist.',
    });
  });

  it('Should return a js object with an url of the requested file ', async () => {
    minIoRepositoryMock.getObjectUrl.mockReturnValueOnce({ url: 'url' });

    const file = await retrievalFileService.getUrl('test', '/test/test_sample1');

    expect(file.url).toBeDefined();
  });

  it('GetUrl > Should return the requeted file, when the file was not found', async () => {
    minIoRepositoryMock.getObjectUrl.mockReturnValueOnce(null);

    let error;
    try {
      await retrievalFileService.getUrl('test', '/test/test_sample1');
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON).toEqual({
      error: 'The file does not exist.',
      detail: 'The file does not exist.',
    });
  });

  it('Should return the object returned from the download method, when the value of the "alt" is "media"', async () => {
    retrievalFileService.download = jest.fn(() => ({ stream: '' }));
    retrievalFileService.getUrl = jest.fn(() => ({ url: 'url' }));

    const file = await retrievalFileService.handle('test', 'path', 'media');
    expect(file).toEqual({
      stream: '',
    });
  });

  it('Should return the object returned from the GetUrl method, when the value of the "alt" is "url"', async () => {
    retrievalFileService.download = jest.fn(() => ({ stream: '' }));
    retrievalFileService.getUrl = jest.fn(() => ({ url: 'url' }));

    const file = await retrievalFileService.handle('test', 'path', 'url');
    expect(file).toEqual({
      url: 'url',
    });
  });

  it('Should return an error, when the alt is not entered', async () => {
    let error;
    try {
      await retrievalFileService.handle('test', 'path', undefined);
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON).toEqual({
      error: 'The "alt" param is required',
      detail: 'The "alt" param is required',
    });
  });

  it('Should return an error, when the value of the alt is invalid', async () => {
    let error;
    try {
      await retrievalFileService.handle('test', 'path', 'invalid');
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON).toEqual({
      error: 'The "alt" param is invalid',
      detail: 'The value of the "alt" parameter must be "media" or "url".',
    });
  });

  it('Should throw an error, when Path Validator fails ', async () => {
    mockValidator.validate.mockImplementationOnce(() => { throw new Error('Error'); });

    let error;
    try {
      await retrievalFileService.handle('test', 'path', 'media');
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
