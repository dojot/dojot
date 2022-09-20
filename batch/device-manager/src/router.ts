import { Router } from "express";
import { makeValidateBody } from "express-class-validator";
import { Keycloak } from "keycloak-connect";
import { DevicesBatchController } from "./controller/devices_batch";
import { TemplatesBatchController } from "./controller/templates_batch";

const routes: Router = Router()
const devices_batch = new DevicesBatchController();
const templates_batch = new TemplatesBatchController();

//Routes
routes.get("/devices", devices_batch.handler);
routes.put("/devices", devices_batch.renove);
routes.post("/devices", devices_batch.create);
routes.post("/devices/csv", devices_batch.create_csv);


routes.get("/templates", templates_batch.handler);
routes.put("/templates", devices_batch.renove);


export { routes };