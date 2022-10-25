// Here the unit tests will be written.
import { describe, expect, test } from '@jest/globals';
import { DevicesValidation } from '../../../../src/app/validations/Device.validations';

describe('Test Validation input body Remove in bath devices', () => {
  describe('remove', () => {
    const schema = DevicesValidation.remove();

    const bodyFactory = (props: { [key: string]: unknown } = {}) => {
      return {
        devices: [],
        ...props,
      };
    };

    it('should fail when body is an empty object or undefined', () => {
      expect(schema.validate({}).error).toBeTruthy();
      expect(schema.validate({ body: {} }).error).toBeTruthy();
    });

    it('should fail if devices array is empty', () => {
      const result = schema.validate({ body: bodyFactory({ devices: [] }) });
      expect(result.error).toBeTruthy();
    });

    it('should fail if devices array is number', () => {
      const result = schema.validate({ body: bodyFactory({ devices: [11] }) });
      expect(result.error).toBeTruthy();
    });
  });
});
