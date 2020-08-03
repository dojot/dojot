const Sanitizer = require('../../../../lib/configManager/fileManager/Sanitizer');

describe('sanitize', () => {
  it('should remove comments', () => {
    const data = '# a comment\napp.hostname=a value';
    const sanitized = Sanitizer.sanitize(data);
    expect(sanitized).toEqual(['app.hostname=a value']);
  });

  it('should remove empty lines', () => {
    const data = '\n\n\napp.hostname=a value';
    const sanitized = Sanitizer.sanitize(data);
    expect(sanitized).toEqual(['app.hostname=a value']);
  });
});
