"""
Higher-level certification encapsulation.
"""
from src.utils import Utils
from src.ejbca.certificate import Certificate

class Thing():
    """
    Device certification data high-level class.
    """
    def __init__(self, tenant: str, device_id: str):
        thing_id = Utils.create_thing_id(tenant, device_id)

        self.tenant = tenant
        self.device_id = device_id
        self.cert = Certificate(self.device_id)
        self.thing_id = thing_id
        self.private_key = self.cert.key["pem"]
        self.thing_certificate = self.cert.crt["pem"]

    def renew_cert(self) -> None:
        """
        Renew a certificate.
        """
        self.cert.renew_cert()
        self.private_key = self.cert.key["pem"]
        self.thing_certificate = self.cert.crt["pem"]

    def get_args_in_dict(self) -> dict:
        """
        Returns the ID, private key and certificate in a dict.
        """
        return {
            "thing_id": self.thing_id,
            "private_key": self.private_key,
            "thing_certificate": self.thing_certificate
        }
