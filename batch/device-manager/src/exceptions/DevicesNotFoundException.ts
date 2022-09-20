import { json } from "express";
import { DevicesErrorBatch } from "src/dto/devices-error-batch";
import HttpException from "./HttpException";
 
class DevicesNotFoundException extends HttpException {
 
 
  constructor(id: string,label: string) {
    const devicesErrorBatch = new DevicesErrorBatch();
    devicesErrorBatch.id = id;
    devicesErrorBatch.label = label;
    devicesErrorBatch.type = "NOT_FOUND";
    devicesErrorBatch.message= "";
    super(400, JSON.stringify(devicesErrorBatch));
  }
}
 
export default DevicesNotFoundException;