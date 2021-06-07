package com.github.dojot.keycloak.providers;

import com.github.dojot.keycloak.error.DojotErrorHandler;
import org.jboss.resteasy.core.ThreadLocalResteasyProviderFactory;
import org.jboss.resteasy.spi.ResteasyProviderFactory;
import org.keycloak.Config;
import org.keycloak.models.KeycloakSessionFactory;
import org.keycloak.provider.ProviderFactory;
import org.keycloak.provider.ServerInfoAwareProviderFactory;

import java.util.ServiceLoader;

/**
 * Factory of service providers for integration with Dojot
 * <p>
 * Classes like this need to be registered as services in META-INF/services/
 *
 * @link https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html
 */
public interface DojotProviderFactory extends ProviderFactory<DojotProvider>, ServerInfoAwareProviderFactory {

    @Override
    default void init(Config.Scope scope) {
        // Register an error handler for custom exceptions for the dojot module
        ((ThreadLocalResteasyProviderFactory) ResteasyProviderFactory.getInstance())
                .getDelegate()
                .registerProvider(DojotErrorHandler.class);
    }

    @Override
    default void postInit(KeycloakSessionFactory keycloakSessionFactory) {
        // register event listeners responsible for capturing
        // data to be passed to DojotProvider...
        for (EventListener listener : ServiceLoader.load(EventListener.class, getClass().getClassLoader())) {
            keycloakSessionFactory.register(listener);
        }
    }
}
