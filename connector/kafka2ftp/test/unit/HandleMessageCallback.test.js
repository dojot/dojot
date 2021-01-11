const { createCallbackToHandleMsgAndUpload } = require('../../app/HandleMessageCallback');

jest.mock('../../app/ReadStream');


describe('Testing FTPClient', () => {
  beforeAll(() => {
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });


  it('Should call correctly ', async () => {
    const mockCallback = jest.fn((x, y) => y + x);
    const callbackTest = createCallbackToHandleMsgAndUpload(mockCallback);

    const data = {
      value: JSON.stringify({
        data: { filename: 'file.txt', encoding: 'utf8', content: 'Hello' },
      }),
    };

    callbackTest(data);
    expect(mockCallback.mock.calls.length).toBe(1);
    expect(mockCallback.mock.calls[0][0]).toBe('file.txt');
  });

  it('Should not call correctly ', async () => {
    const mockCallback = jest.fn().mockImplementation(() => {
      throw new Error('Test');
    });
    const callbackTest = createCallbackToHandleMsgAndUpload(mockCallback);

    const data = {
      value: JSON.stringify({
        data: { filename: 'file.txt', encoding: 'utf8', content: 'Hello' },
      }),
    };

    try {
      callbackTest(data);
    } catch (e) {
      expect(e.message).toBe('Test');
    }

    expect(mockCallback.mock.calls.length).toBe(1);
    expect(mockCallback.mock.calls[0][0]).toBe('file.txt');
  });
});
