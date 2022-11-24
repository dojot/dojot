-- AlterTable
ALTER TABLE "report_attempt" ALTER COLUMN "error" DROP NOT NULL,
ALTER COLUMN "failed_at" DROP NOT NULL,
ALTER COLUMN "canceled_at" DROP NOT NULL,
ALTER COLUMN "finished_at" DROP NOT NULL;
