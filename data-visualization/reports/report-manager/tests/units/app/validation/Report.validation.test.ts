import { ReportValidation } from 'src/app/validations'

describe('Report.validation', () => {
  describe('findMany', () => {
    const schema = ReportValidation.findMany()

    it('should pass with no query params', () => {
      const result = schema.validate({})
      expect(result.error).toBeUndefined()
    })

    it('should pass when passing page and pageSize on query params', () => {
      const result = schema.validate({ query: { page: 1, pageSize: 10 } })
      expect(result.error).toBeUndefined()
    })

    it('should pass if page or pageSize are numeric strings', () => {
      const result = schema.validate({ query: { page: '1', pageSize: '10' } })
      expect(result.error).toBeUndefined()
    })

    it('should fail if page or pageSize are not numbers', () => {
      const result1 = schema.validate({ query: { page: 'a', pageSize: 'b' } })
      const result2 = schema.validate({ query: { page: {}, pageSize: {} } })
      const result3 = schema.validate({ query: { page: true, pageSize: true } })
      expect(result1.error).toBeTruthy()
      expect(result2.error).toBeTruthy()
      expect(result3.error).toBeTruthy()
    })

    it('should pass if name is a string', () => {
      const result = schema.validate({ query: { name: 'name' } })
      expect(result.error).toBeUndefined()
    })

    it('should fail if name is not a string', () => {
      const result1 = schema.validate({ query: { name: 100 } })
      const result2 = schema.validate({ query: { name: {} } })
      const result3 = schema.validate({ query: { name: true } })
      const result4 = schema.validate({ query: { name: [] } })
      expect(result1.error).toBeTruthy()
      expect(result2.error).toBeTruthy()
      expect(result3.error).toBeTruthy()
      expect(result4.error).toBeTruthy()
    })
  })

  describe('findById', () => {
    const schema = ReportValidation.findById()

    it('should fail if do not pass an ID on params', () => {
      expect(schema.validate({}).error).toBeTruthy()
      expect(schema.validate({ params: {} }).error).toBeTruthy()
    })

    it('should the ID be a string', () => {
      expect(schema.validate({ params: { id: '123' } }).error).toBeUndefined()
      expect(schema.validate({ params: { id: 'abc' } }).error).toBeUndefined()

      expect(schema.validate({ params: { id: {} } }).error).toBeTruthy()
      expect(schema.validate({ params: { id: false } }).error).toBeTruthy()
      expect(schema.validate({ params: { id: 12345 } }).error).toBeTruthy()
    })
  })

  describe('delete', () => {
    const schema = ReportValidation.delete()

    it('should fail if do not pass an ID on params', () => {
      expect(schema.validate({}).error).toBeTruthy()
      expect(schema.validate({ params: {} }).error).toBeTruthy()
    })

    it('should the ID be a string', () => {
      expect(schema.validate({ params: { id: '123' } }).error).toBeUndefined()
      expect(schema.validate({ params: { id: 'abc' } }).error).toBeUndefined()

      expect(schema.validate({ params: { id: {} } }).error).toBeTruthy()
      expect(schema.validate({ params: { id: false } }).error).toBeTruthy()
      expect(schema.validate({ params: { id: 12345 } }).error).toBeTruthy()
    })
  })
})
