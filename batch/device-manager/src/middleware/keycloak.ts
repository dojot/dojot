import { WebUtils } from "@dojot/microservice-sdk";
import app from "src/app";

let _keycloakSessionClient;

const keycloakClientSession = null;

function initKeycloak (): void {
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
            database: process.env.KEYCLOAK_URI
            //error:e.name
          });      
        }


  }


function getKeycloak() {
      if(keycloakClientSession)
      {

      }   
      return _keycloakSessionClient; 
}


module.exports = {
    initKeycloak,
    getKeycloak
};