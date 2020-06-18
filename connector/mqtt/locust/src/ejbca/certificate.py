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

    def __init__(self, thing_id):
        self.logger = Utils.create_logger("certificate")
        Utils.validate_thing_id(thing_id)

        self.jwt = RedisClient().get_jwt()

        self.c_name = thing_id
        self.key = {"pem": self.generate_private_key()}
        self.csr = {"pem": self.generate_csr()}
        self.crt = {}
        self.crt["fingerprint"], self.crt["pem"] = self.sign_cert()

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

        return crypto.dump_certificate_request(crypto.FILETYPE_PEM, req).decode("ascii")

    def sign_cert(self) -> str:
        """
        Sign the certificates.

        Returns the pem certificate.
        """
        # we remove the last from the csr \n
        csr = self.csr["pem"][:-1]
        return DojotAPI.sign_cert(self.jwt, csr)

    def renew_cert(self) -> None:
        """
        Renew a certificate.

        The procedure made here is described in:
        https://doc.primekey.com/ejbca/ejbca-operations/ejbca-operations-guide/ca-operations-guide/end-entities/certificate-renewal
        """
        DojotAPI.reset_entity_status(self.jwt, self.crt["fingerprint"])

        self.key = {"pem": self.generate_private_key()}
        self.csr = {"pem": self.generate_csr()}
        self.crt = {}
        self.crt["pem"] = self.sign_cert()
