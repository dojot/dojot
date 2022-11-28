// Here the unit tests will be written.
import { describe, expect } from '@jest/globals';
import { TemplatesValidation } from '../../../../src/app/validations/Template.validations';

describe('Test Validation input body Remove in bath templates', () => {
  describe('remove', () => {
    const schema = TemplatesValidation.remove();

    const bodyFactory = (props: { [key: string]: unknown } = {}) => {
      return {
        templates: [],
        ...props,
      };
    };

    it('should fail when body is an empty object or undefined', () => {
      expect(schema.validate({}).error).toBeTruthy();
      expect(schema.validate({ body: {} }).error).toBeTruthy();
    });

    it('should fail if devices array is empty', () => {
      const result = schema.validate({ body: bodyFactory({ templates: [] }) });
      expect(result.error).toBeTruthy();
    });
  });
});
