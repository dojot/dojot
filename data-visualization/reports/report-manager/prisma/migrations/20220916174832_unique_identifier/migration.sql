/*
  Warnings:

  - A unique constraint covering the columns `[identifier]` on the table `report_type` will be added. If there are existing duplicate values, this will fail.

*/
-- CreateIndex
CREATE UNIQUE INDEX "report_type_identifier_key" ON "report_type"("identifier");
