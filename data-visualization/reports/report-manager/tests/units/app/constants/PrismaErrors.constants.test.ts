import { PRISMA_ERRORS } from 'src/app/constants'

describe('PrismaErrors.constants', () => {
  it('should all keys be uppercase strings', () => {
    const allKeysAreValid = Object.keys(PRISMA_ERRORS).every((key) => {
      const isKeyString = typeof key === 'string'
      const isKeyUppercase = key === key.toUpperCase()
      return isKeyString && isKeyUppercase
    })

    expect(allKeysAreValid).toBe(true)
  })

  it('should all values have the letter P followed by four numbers', () => {
    const regex = new RegExp('^P[0-9]{4}$')

    const allValuesAreValid = Object.values(PRISMA_ERRORS).every((value) =>
      value.match(regex),
    )

    expect(allValuesAreValid).toBe(true)
  })
})
