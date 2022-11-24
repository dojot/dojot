/* eslint-disable @typescript-eslint/no-var-requires */

const { PrismaClient } = require('@prisma/client')

const ReportTypes = require('./seeds/ReportTypes.seed.json')

const prisma = new PrismaClient()

const createReportTypes = async () => {
  return prisma.reportType.createMany({
    data: ReportTypes,
    skipDuplicates: true,
  })
}

;(async () => {
  try {
    await createReportTypes()
  } catch (e) {
    console.error(e)
    await prisma.$disconnect()
    process.exit(1)
  }
})()
