import express from 'express'
import cors from 'cors'
import { routes } from './router'
import bodyParser from "body-parser";
import { Logger, WebUtils } from '@dojot/microservice-sdk';
import KafkaConsumer from './kafka/consumer/kafka-consumer';

class App {
  public express: express.Application
 
  public constructor (
    private logger: Logger,
    private config: any,
    private kafkaConsumer: KafkaConsumer,
    private tenantManager: any,
  ) {
    // dotenv.config();
    this.express = express();
    this.logger.info('Create Constructor App',{});
  }

  public async init() {
    this.kafka();
    await this.kafkaConsumer.init();
    this.kafkaConsumer.initNewTenantEvent(this.tenantManager.create.bind(this.tenantManager))
    // this.database();
    // this.middlewares();
    this.routes();
  }

  private kafka() : void {
    
  }

  private middlewares (): void {
    const {createKeycloakAuthInterceptor} = WebUtils.framework.interceptors
    this.express.use(bodyParser.json());
    this.express.use(bodyParser.urlencoded({ extended: true }));
    this.express.use(express.json());
    this.express.use(cors());
    const keycloakInterceptor = createKeycloakAuthInterceptor(this.tenantManager.tenants, this.logger, '/')
    // this.express.use(keycloakInterceptor)
  }

  // private async database (): Promise<void> {
  //     this.logger.info(`The connection database URL is `,{
  //     url: process.env.DATABASE_URL},
  //   );
  //   try {
  //      await prismaClient.$connect();  
  //   } catch (error) {
  //     this.logger.error(`ERROR Connected Database`,{
  //       database: process.env.DATABASE_URL,
  //       error:error.name
  //     });
  //   } 
  // }

  private routes (): void {
    //this.logger.info('>>>', this.keycloakClientSession.getTokenSet());
    this.express.use(routes)
  }
 }

export default App