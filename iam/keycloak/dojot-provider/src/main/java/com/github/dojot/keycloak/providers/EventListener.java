package com.github.dojot.keycloak.providers;

import org.keycloak.provider.ProviderEventListener;

/**
 * A Service Provider Interface for event listeners of interest to Dojot
 * <p>
 * Classes like this need to be registered as services in META-INF/services/
 *
 * @link https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html
 */
public interface EventListener extends ProviderEventListener {
}
