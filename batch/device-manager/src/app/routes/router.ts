import { Logger } from "@dojot/microservice-sdk";
import { PrismaClient } from "@prisma/client";
import { Router } from "express";
import { DevicesBatchController } from "../controller/devices_batch";
import { TemplatesBatchController } from "../controller/templates_batch";

/*
const routes: Router = Router()

const logger = new Logger('device-manager-batch');
const prismaClientConnection = new PrismaClient({
    log: ["error", "info", "query", "warn"],
  });
  

//const devicesServices = new DevicesServices(logger,prismaClientConnection);
//const templatesSevices = new TemplatesServices(logger,prismaClientConnection);

const devices_batch = new DevicesBatchController(logger,devicesServices);
const templates_batch = new TemplatesBatchController(logger,templatesSevices);

//Routes
routes.put("/devices", devices_batch.renove.bind(devices_batch));
routes.post("/devices", devices_batch.create.bind(devices_batch));
routes.post("/devices/csv", devices_batch.create_csv.bind(devices_batch));

routes.put("/templates", templates_batch.remove.bind(templates_batch));

export { routes };*/