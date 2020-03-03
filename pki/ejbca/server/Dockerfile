FROM primekey/ejbca-ce:6.15.2.5

COPY --chown=10001:0 bin/after-deployed-pre.sh \
                     bin/before-shutdown.sh \
                     bin/configuration-variables.sh \
                     bin/smtp-mail-setup.sh \
                     bin/import-profiles.sh \
                     bin/certificate-authorities-creation.sh \
                     bin/users-setup-creation.sh \
                     bin/appserver-tls-certificate-generation.sh \
                     bin/microservices-end-entities-creation.sh \
                     bin/services-creation.sh \
                    /opt/primekey/bin/internal/

COPY --chown=10001:0 resources/certprofiles /mnt/persistent/certprofiles

COPY --chown=10001:0 resources/entityprofiles /mnt/persistent/entityprofiles
