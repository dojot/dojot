-- DropForeignKey
ALTER TABLE "report" DROP CONSTRAINT "report_type_id_fkey";

-- DropForeignKey
ALTER TABLE "report_attempt" DROP CONSTRAINT "report_attempt_report_id_fkey";

-- DropForeignKey
ALTER TABLE "report_file" DROP CONSTRAINT "report_file_report_id_fkey";

-- AddForeignKey
ALTER TABLE "report" ADD CONSTRAINT "report_type_id_fkey" FOREIGN KEY ("type_id") REFERENCES "report_type"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "report_file" ADD CONSTRAINT "report_file_report_id_fkey" FOREIGN KEY ("report_id") REFERENCES "report"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "report_attempt" ADD CONSTRAINT "report_attempt_report_id_fkey" FOREIGN KEY ("report_id") REFERENCES "report"("id") ON DELETE CASCADE ON UPDATE CASCADE;
