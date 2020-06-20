"""
Certificate generation.
"""
from OpenSSL import crypto

from src.config import CONFIG
from src.utils import Utils
from src.mqtt_locust.redis_client import RedisClient
from src.dojot.api import DojotAPI

class Certificate:
    """
    Generates a certificate and private key for a device.
    """

    def __init__(self, device_id):
        self.logger = Utils.create_logger("certificate")
        # Utils.validate_thing_id(thing_id)

        self.jwt = RedisClient().get_jwt()

        self.c_name = device_id
        self.key = {"pem": self.generate_private_key()}
        self.csr = {"pem": self.generate_csr()}
        self.crt = {}
        self.crt["fingerprint"], self.crt["pem"] = self.generate_certificate()

    def generate_private_key(self) -> str:
        """
        Generates the private key in a string.
        """
        try:
            bit_len = 2048
            key = crypto.PKey()
            key.generate_key(crypto.TYPE_RSA, bit_len)
            return crypto.dump_privatekey(crypto.FILETYPE_PEM, key).decode("utf-8")
        except Exception as exception:
            self.logger.error("Error while generating the private key.")
            self.logger.error("Error: %s", exception)
            raise

    def generate_csr(self) -> str:
        """
        Generates the CSR in a string.
        """
        # based on https://github.com/cjcotton/python-csr

        req = crypto.X509Req()
        req.get_subject().CN = self.c_name

        key = crypto.load_privatekey(crypto.FILETYPE_PEM, self.key["pem"])

        req.set_pubkey(key)
        req.sign(key, "sha256")

        # Remove \n from CSR and return the csr
        return crypto.dump_certificate_request(crypto.FILETYPE_PEM, req).decode("ascii")[:-1]

    def generate_certificate(self) -> str:
        """
        Generate the certificates.

        Returns the pem certificate.
        """
        return DojotAPI.generate_certificate(self.jwt, self.csr["pem"])

    def renew_cert(self) -> None:
        """
        Renew a certificate.
        """
        self.revoke_cert()

        self.key = {"pem": self.generate_private_key()}
        self.csr = {"pem": self.generate_csr()}
        self.crt = {}
        self.crt["fingerprint"], self.crt["pem"] = self.generate_certificate()

    def revoke_cert(self) -> None:
        """
        Revoke a certificate
        """
        DojotAPI.revoke_certificate(self.jwt, self.crt["fingerprint"])
