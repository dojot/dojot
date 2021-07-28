const { getExpectedResponseFormat } = require('../../app/express/helpers/AcceptHeaderHelper');

describe('AcceptHeaderHelper', () => {
  test('should return the value "json", when receiving the value "Aplication/json" in the accept header', () => {
    const acceptHeader = 'Aplication/json';
    const returnValue = getExpectedResponseFormat(acceptHeader);
    expect(returnValue).toBe('json');
  });

  test('should return the value "json", when receiving an empty value in the accept header', () => {
    const acceptHeader = '';
    const returnValue = getExpectedResponseFormat(acceptHeader);
    expect(returnValue).toBe('json');
  });

  test('should return the value "json", when receiving an undefined value in the accept header', () => {
    const acceptHeader = undefined;
    const returnValue = getExpectedResponseFormat(acceptHeader);
    expect(returnValue).toBe('json');
  });

  test('should return the value "csv", when receiving the value "Text/csv" in the accept header', () => {
    const acceptHeader = 'Text/csv';
    const returnValue = getExpectedResponseFormat(acceptHeader);
    expect(returnValue).toBe('csv');
  });
});
