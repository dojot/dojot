# Based on https://github.com/gbbirkisson/kong-plugin-jwt-keycloak/blob/master/Dockerfile that is licensed under Apache-2.0 License
FROM kong:2.3-alpine as builder

USER root

# Getting the code from repository of
# Kong plugin jwt-keycloak that is licensed under Apache-2.0 License
#
# instead of using the luarocks publish lib
# (https://luarocks.org/modules/gbbirkisson/kong-plugin-jwt-keycloak),
# we are using the code directly from the repository because the version that supports
# regex at the issuer has not yet been published.
ENV GIT_PLUGIN_JWT_KEYCLOAK_REPO_URL=https://github.com/gbbirkisson/kong-plugin-jwt-keycloak.git
ENV GIT_PLUGIN_JWT_KEYCLOAK_COMMIT_OR_VERSION=9a5728c0858ded473e1032e0219653d9502b0e16

RUN apk add --no-cache git zip

WORKDIR /tmp

# Getting code from the jwt-keycloak plugin
RUN git clone ${GIT_PLUGIN_JWT_KEYCLOAK_REPO_URL}

WORKDIR /tmp/kong-plugin-jwt-keycloak

# Getting code from the commit or version
RUN git checkout ${GIT_PLUGIN_JWT_KEYCLOAK_COMMIT_OR_VERSION}

# Installing the plugin
RUN luarocks make && luarocks pack kong-plugin-jwt-keycloak

USER kong

# Create final image
FROM kong:2.3-alpine

# Copy default config
COPY kong.conf /etc/kong/

USER root

# Copy and install the jwt-keycloak plugin
COPY --from=builder /tmp/kong-plugin-jwt-keycloak/*.rock /tmp/
RUN luarocks install /tmp/kong-plugin-jwt-keycloak* && rm /tmp/*

# Copy and install the dojot plugin, the pepkong
WORKDIR /custom-plugins/pepkong
COPY ./plugins/pepkong /custom-plugins/pepkong
RUN luarocks make

USER kong

HEALTHCHECK --start-period=2m --interval=30s --timeout=10s --retries=3 \
    CMD kong health || exit 1