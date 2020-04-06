"""
Certificate generation.
"""
import json
import requests
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
        self.key = {"raw": "", "pem": self.generate_private_key()}
        self.csr = {"raw": "", "pem": self.generate_csr()}
        self.crt = {"raw": "", "pem": ""}
        self.sign_cert()
        DojotAPI.create_ejbca_user(self.jwt, self.c_name)
        self.crt["pem"] = self.save_crt()

    def save_crt(self) -> str:
        """
        Generate the CRT in a string.
        """
        return ("-----BEGIN CERTIFICATE-----\n"
                + self.crt["raw"]
                + "\n-----END CERTIFICATE-----\n")

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

        dnsname = CONFIG['security']['dns_cert']

        dns = []
        for i in dnsname:
            dns.append("DNS: %s" % i)
        dns = ", ".join(dns)

        req = crypto.X509Req()
        req.get_subject().CN = self.c_name

        # Add in extensions

        base_constraints = ([
            crypto.X509Extension(b"keyUsage",
                                 False,
                                 b"Digital Signature, Non Repudiation, Key Encipherment"),
            crypto.X509Extension(b"basicConstraints",
                                 False,
                                 b"CA:FALSE"),
        ])

        x509_extensions = base_constraints

        if dns:
            san_constraint = crypto.X509Extension(b"subjectAltName", False, dns.encode("utf-8"))
            x509_extensions.append(san_constraint)

        req.add_extensions(x509_extensions)

        key = crypto.load_privatekey(crypto.FILETYPE_PEM, self.key["pem"])

        req.set_pubkey(key)
        req.sign(key, "sha256")

        return crypto.dump_certificate_request(crypto.FILETYPE_PEM, req).decode("ascii")

    def sign_cert(self) -> None:
        """
        Sign the certificates.
        """
        csr = self.csr["pem"]
        cut_down_clr = (csr[csr.find('-----BEGIN CERTIFICATE REQUEST-----')
                            + len('-----BEGIN CERTIFICATE REQUEST-----'):
                            csr.find("-----END CERTIFICATE REQUEST-----")]
                        .replace("\n", ""))

        req = json.dumps({
            "passwd": "dojot",
            "certificate": cut_down_clr
        })

        default_header = {
            'content-type': 'application/json',
            'Accept': 'application/json',
            "Authorization": "Bearer {0}".format(self.jwt),
        }
        url = CONFIG['dojot']['url'] + "/sign/" + self.c_name + "/pkcs10"

        response = None
        try:
            response = requests.post(
                url,
                headers=default_header,
                data=req
            )
            response.raise_for_status()

        except Exception as exception:
            self.logger.error(str(exception))
            raise

        else:
            self.crt["raw"] = json.loads(response.content)['status']['data']

        response.connection.close()


    def reset_entity_status(self, status: int = 10) -> None:
        """
        Changes the entity's status.

        Params:
            status: status to be set. Defaults to 10.
        """
        url = CONFIG['dojot']['url'] + "/user"

        default_header = {
            'content-type': 'application/json',
            'Accept': 'application/json',
            "Authorization": "Bearer {0}".format(self.jwt),
        }

        data = {
            "username": self.c_name,
            "password": "dojot",
            "subjectDN": "CN=" + self.c_name,
            "status": status
        }

        response = None
        try:
            response = requests.post(
                url,
                headers=default_header,
                data=json.dumps(data)
            )
            response.raise_for_status()

        except Exception as exception:
            self.logger.error(str(exception))
            raise

        response.connection.close()

    def renew_cert(self) -> None:
        """
        Renew a certificate.

        The procedure made here is described in:
        https://doc.primekey.com/ejbca/ejbca-operations/ejbca-operations-guide/ca-operations-guide/end-entities/certificate-renewal
        """
        self.reset_entity_status()
        self.key["pem"] = self.generate_private_key()
        self.csr["pem"] = self.generate_csr()
        self.sign_cert()
        self.crt["pem"] = self.save_crt()
