"""
Certificate utilities.
"""
import os
import requests
from OpenSSL import crypto

from src.config import CONFIG
from src.utils import Utils
from src.ejbca.thing import Thing


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
        # Loads the certificate as a X509 object
        cert: crypto.X509 = crypto.load_certificate(
            crypto.FILETYPE_PEM, thing.thing_certificate)
        # Retrieves the Serial Number in Hexadecimal
        serial_number = hex(cert.get_serial_number())[2:]
        # URL to revoke a certificate
        url = CONFIG["security"]["ejbca_url"] + \
            "/ca/CN={0},O=EJBCA/certificate/{1}".format(
                CONFIG["security"]["ejbca_ca_name"], serial_number)

        requests.delete(url)

    @staticmethod
    def has_been_revoked(thing: Thing) -> bool:
        """
        Verifies whether the certificate has been revoked or not.
        """
        # Loads the certificate as a X509 object
        cert: crypto.X509 = crypto.load_certificate(
            crypto.FILETYPE_PEM, thing.thing_certificate)
        # Retrieves the Serial Number in Hexadecimal
        serial_number = hex(cert.get_serial_number())[2:]
        # URL to verify the certificate status
        url = CONFIG["security"]["ejbca_url"] + \
            "/ca/CN={0},O=EJBCA/certificate/{1}/status".format(
                CONFIG["security"]["ejbca_ca_name"], serial_number)

        res = requests.get(url)
        res_json = res.json()

        if res_json.get("status") and res_json["status"].get("return"):
            return res_json["status"]["return"]["reason"] == 0

        LOGGER.error("Error: invalid response from EJBCA")
        return False
