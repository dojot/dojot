"""
Certificate utilities.
"""
import os
import requests
from OpenSSL import crypto

from src.config import CONFIG
from src.utils import Utils
from src.ejbca.thing import Thing
from src.mqtt_locust.redis_client import RedisClient


LOGGER = Utils.create_logger("cert_utils")


class CertUtils:
    """
    Handles certificate-related operations.
    """

    @staticmethod
    def get_private_key_file(device_id: str) -> str:
        """
        Creates the key filename.
        """
        Utils.validate_device_id(device_id)
        return "{0}.key".format(device_id)

    @staticmethod
    def get_certificate_file(device_id: str) -> str:
        """
        Creates the certificate filename.
        """
        Utils.validate_device_id(device_id)
        return "{0}.crt".format(device_id)

    @staticmethod
    def create_cert_files(thing: Thing, directory: str = "/cert/") -> None:
        """Creates the .key and .crt files for a device.

        Args:
            device_id: device's identification.
            thing: Thing object with certificate's info.
            directory: directory to save the files.
        """
        key_path = directory + CertUtils.get_private_key_file(thing.device_id)
        cert_path = directory + CertUtils.get_certificate_file(thing.device_id)

        try:
            if os.path.isfile(key_path):
                os.remove(key_path)
            with open(key_path, "w") as key_file:
                key_file.write(str(thing.private_key))

            if os.path.isfile(cert_path):
                os.remove(cert_path)
            with open(cert_path, "w") as key_file:
                key_file.write(str(thing.thing_certificate))

        except Exception as exception:
            LOGGER.error("Error: %s", str(exception))
            raise

    @staticmethod
    def new_cert(tenant: str, device_id: str) -> Thing:
        """
        Creates/renovates the certificate for a device.
        """
        Utils.validate_tenant(tenant)
        Utils.validate_device_id(device_id)
        thing = Thing(tenant, device_id)

        return thing

    @staticmethod
    def revoke_cert(thing: Thing) -> None:
        """
        Revokes a certificate for a specific device.
        """
        thing.cert.revoke_cert()

    @staticmethod
    def has_been_revoked(thing: Thing) -> bool:
        """
        Verifies whether the certificate has been revoked or not.
        """
        url = CONFIG["dojot"]["url"]+ "x509/v1/certificates/" + thing.cert.crt['fingerprint']

        res = requests.get(
            url=url,
            headers={
                "Authorization": "Bearer {0}".format(RedisClient().get_jwt())
            }
        )

        return res.status_code == 404
