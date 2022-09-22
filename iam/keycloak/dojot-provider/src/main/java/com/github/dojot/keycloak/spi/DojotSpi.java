package com.github.dojot.keycloak.spi;

import com.github.dojot.keycloak.providers.DojotProvider;
import com.github.dojot.keycloak.providers.DojotProviderFactory;
import org.keycloak.provider.Spi;

/**
 * Service Provider Interface for integration with Dojot
 * <p>
 * Classes like this need to be registered as services in META-INF/services/
 *
 * @link https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html
 */
public class DojotSpi implements Spi {
    @Override
    public boolean isInternal() {
        return false;
    }

    @Override
    public String getName() {
        return "dojot";
    }

    @Override
    public Class<DojotProvider> getProviderClass() {
        return DojotProvider.class;
    }

    @Override
    public Class<DojotProviderFactory> getProviderFactoryClass() {
        return DojotProviderFactory.class;
    }
}
