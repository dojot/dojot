import { Logger, WebUtils } from '@dojot/microservice-sdk';
import * as KafkaUtils from '../utils/kafkaPayloadUtil'

export default class TenantManagerService {
  public tenants: WebUtils.TenantInfo[];
  constructor(private logger: Logger){
    this.tenants = [];
  }

  update() {

  }

  create(payload: any, ack: Function){
    const payloadObject = KafkaUtils.getValue(payload);
    this.logger.info(`${payloadObject.type} bucket for ${payloadObject.tenant} tenant`, {});
  }
}