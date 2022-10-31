package com.github.dojot.keycloak.kafka;

import java.io.Serializable;

/**
 * Enumerator of events known by the dojot platform
 */
public enum Event implements Serializable {
    CREATE, DELETE
}