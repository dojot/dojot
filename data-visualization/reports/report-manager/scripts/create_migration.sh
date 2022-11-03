#!/bin/sh

sudo docker-compose -f docker/migration.yml up -d
export DATABASE_URL=postgresql://postgres:postgres@localhost:4769/report-manager?schema=public
yarn prisma migrate dev --skip-seed
sudo docker-compose -f docker/migration.yml down
