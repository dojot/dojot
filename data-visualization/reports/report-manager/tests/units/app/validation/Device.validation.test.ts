import { REPORT_FORMATS } from 'src/app/constants'
import { DeviceValidation } from 'src/app/validations'

describe('Report.validation', () => {
  describe('create', () => {
    const schema = DeviceValidation.create()

    const fakeAttr = {
      id: 1,
      label: 'Attr 1',
      type: 'dynamic',
      valueType: 'string',
    }

    const fakeDevice = {
      id: 'abc123',
      label: 'Device name',
      attrs: [fakeAttr],
    }

    const bodyFactory = (props: { [key: string]: unknown } = {}) => {
      return {
        name: 'Report name',
        format: REPORT_FORMATS.PDF,
        devices: [fakeDevice],
        ...props,
      }
    }

    it('should fail when body is an empty object or undefined', () => {
      expect(schema.validate({}).error).toBeTruthy()
      expect(schema.validate({ body: {} }).error).toBeTruthy()
    })

    it('should pass with all required data', () => {
      const result = schema.validate({ body: bodyFactory() })
      expect(result.error).toBeUndefined()
    })

    it('should only allow specific formats', () => {
      Object.values(REPORT_FORMATS).forEach((format) => {
        const result = schema.validate({ body: bodyFactory({ format }) })
        expect(result.error).toBeUndefined()
      })

      const nonExistentFormatResult = schema.validate({
        body: bodyFactory({ format: '_does_not_exist_' }),
      })

      expect(nonExistentFormatResult.error).toBeTruthy()
    })

    it('should singleReportFile be a strict boolean', () => {
      const validBodies = [
        bodyFactory({ singleReportFile: false }),
        bodyFactory({ singleReportFile: true }),
      ]

      const inValidBodies = [
        bodyFactory({ singleReportFile: 'no' }),
        bodyFactory({ singleReportFile: 'yes' }),
        bodyFactory({ singleReportFile: 'false' }),
        bodyFactory({ singleReportFile: 'true' }),
      ]

      validBodies.forEach((validBody) => {
        const result = schema.validate({ body: validBody })
        expect(result.error).toBeUndefined()
      })

      inValidBodies.forEach((invalidBody) => {
        const result = schema.validate({ body: invalidBody })
        expect(result.error).toBeTruthy()
      })
    })

    it('should initial and final date be ISO dates', () => {
      const isoDate1 = new Date().toISOString()
      const isoDate2 = new Date('2022-10-10').toISOString()

      const validBodies = [
        bodyFactory({ initialDate: isoDate1, finalDate: isoDate1 }),
        bodyFactory({ initialDate: isoDate2, finalDate: isoDate2 }),
      ]

      const inValidBodies = [
        bodyFactory({ initialDate: {}, finalDate: {} }),
        bodyFactory({ initialDate: 'test', finalDate: 'test' }),
        bodyFactory({ initialDate: 123456, finalDate: 123456 }),
        bodyFactory({ initialDate: new Date(), finalDate: new Date() }),
        bodyFactory({ initialDate: '2022-10-58', finalDate: '2022-10-58' }),
        bodyFactory({ initialDate: '2022-78-10', finalDate: '2022-78-10' }),
      ]

      validBodies.forEach((validBody) => {
        const result = schema.validate({ body: validBody })
        expect(result.error).toBeUndefined()
      })

      inValidBodies.forEach((invalidBody) => {
        const result = schema.validate({ body: invalidBody })
        expect(result.error).toBeTruthy()
      })
    })

    it('should fail if devices array is empty', () => {
      const result = schema.validate({ body: bodyFactory({ devices: [] }) })
      expect(result.error).toBeTruthy()
    })

    it('should fail if attrs array is empty', () => {
      const devices = [{ ...fakeDevice, attrs: [] }]
      const result = schema.validate({ body: bodyFactory({ devices }) })
      expect(result.error).toBeTruthy()
    })
  })
})
