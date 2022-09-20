import express from 'express'
import cors from 'cors'
import { routes } from './router'
import bodyParser from "body-parser";
import { prismaClient } from "src/database/prismaclient";
import  { Logger,WebUtils,Kafka } from  '@dojot/microservice-sdk';
import * as dotenv from 'dotenv'
import e from 'express';

class App {
  public express: express.Application
   
   logger = new Logger('Device-Mmanager-Batch');

   keycloakClientSession = null;

  public constructor () {
    dotenv.config();
    this.express = express();
    this.logger.info('Create Constructor App',{});
    this.keycloak();
    this.database();
   // this.kafka();
    this.middlewares();
    this.routes();
  }

  private middlewares (): void {

    this.express.use(bodyParser.json());
    this.express.use(bodyParser.urlencoded({ extended: true }));
    this.express.use(express.json());
    this.express.use(cors());
    //this.express.use(this.keycloak);
 
  }

  private async database (): Promise<void> {
      this.logger.info(`The connection database URL is `,{
      url: process.env.DATABASE_URL},
    );
    try {
       await prismaClient.$connect();  
    } catch (error) {
      this.logger.error(`ERROR Connected Database`,{
        database: process.env.DATABASE_URL,
        error:error.name
      });      
    }

  
  }

  private kafka (): void {
    this.logger.info(`The connection Kafka URL is `,{
      url: process.env.KAFKA_HOST},
    );
    try {
      Kafka.Producer 
    } catch (error) {
      this.logger.error(`ERROR Create Session KeyCloak`,{
        database: process.env.KEYCLOAK_URI,
        error:e.name
      });      
    }
  }

  private keycloak (): void {
    this.logger.info(`Setup Config to connection keycloakClientSession URL is `,{
      url: process.env.KEYCLOAK_URI,
      tenant: process.env.KEYCLOAK_REALM
    },
    );
        try {
          
           this.keycloakClientSession = new WebUtils.KeycloakClientSession(
            process.env.KEYCLOAK_URI,
            process.env.KEYCLOAK_REALM,
            {
              grant_type: "client_credentials",
              client_id:  process.env.KEYCLOAK_CLIENT_ID,
              client_secret:  process.env.KEYCLOAK_CLIENT_SECRET,
            },
            this.logger,
            {
              retryDelay: 5000,
            },
          );

          // Start session
          this.keycloakClientSession.start().then(() => {

            this.logger.info('Successfully connected to keycloak.',
            {});
            }).catch((error) => {
              this.logger.error(error,{});
            });

            //this.logger.info('>>>',keycloakClientSession.getTokenSet().id_token);


        } catch (error) {
          this.logger.error(`ERROR Create Session KeyCloak`,{
            database: process.env.KEYCLOAK_URI,
            error:e.name
          });      
        }


  }
  
  private routes (): void {
    //this.logger.info('>>>', this.keycloakClientSession.getTokenSet());
    this.express.use(routes)
  }
 }

export default new App()