-- CreateTable
CREATE TABLE "report_type" (
    "id" TEXT NOT NULL,
    "identifier" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "report_type_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "report" (
    "id" TEXT NOT NULL,
    "type_id" TEXT NOT NULL,
    "name" VARCHAR(255) NOT NULL,
    "format" TEXT NOT NULL,
    "single_report_file" BOOLEAN NOT NULL,
    "initial_date" TIMESTAMP(3),
    "final_date" TIMESTAMP(3),
    "params" JSONB NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "report_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "report_file" (
    "id" TEXT NOT NULL,
    "report_id" TEXT NOT NULL,
    "path" TEXT NOT NULL,
    "mime_type" TEXT NOT NULL,
    "filename" TEXT NOT NULL,
    "file_size_kb" DOUBLE PRECISION NOT NULL,
    "expires_at" TIMESTAMP(3) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "report_file_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "report_attempt" (
    "id" TEXT NOT NULL,
    "report_id" TEXT NOT NULL,
    "error" JSONB NOT NULL,
    "failed_at" TIMESTAMP(3) NOT NULL,
    "canceled_at" TIMESTAMP(3) NOT NULL,
    "finished_at" TIMESTAMP(3) NOT NULL,
    "created_at" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "report_attempt_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "report_file_report_id_key" ON "report_file"("report_id");

-- AddForeignKey
ALTER TABLE "report" ADD CONSTRAINT "report_type_id_fkey" FOREIGN KEY ("type_id") REFERENCES "report_type"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "report_file" ADD CONSTRAINT "report_file_report_id_fkey" FOREIGN KEY ("report_id") REFERENCES "report"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "report_attempt" ADD CONSTRAINT "report_attempt_report_id_fkey" FOREIGN KEY ("report_id") REFERENCES "report"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
