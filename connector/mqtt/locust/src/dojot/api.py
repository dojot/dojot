"""
API calls to Dojot.
"""
import json
import requests

from src.config import CONFIG
from src.utils import Utils


LOGGER = Utils.create_logger("api")


class DojotAPI():
    """
    Utility class with API calls to Dojot.
    """
    @staticmethod
    def get_jwt() -> str:
        """
        Request a JWT token.
        """
        LOGGER.info("Retrieving JWT...")

        data = {
            "username": CONFIG['dojot']['user'],
            "passwd": CONFIG['dojot']['passwd'],
        }

        res = requests.post(
            "{0}/auth".format(CONFIG['dojot']['url']),
            data=json.dumps(data),
            headers={"Content-Type": "application/json"},
        )
        res.raise_for_status()

        LOGGER.info(".. retrieved JWT")
        return json.loads(res.text)["jwt"]

    @staticmethod
    def create_devices(jwt: str, template_id: str, n: int, batch: int) -> None:
        """
        Create the devices.

        Parameters:
            jwt: Dojot JWT token
            template_id: template ID to be used by the devices
            n: total number of devices to be created
            batch: number of devices to be created in each iteration
        """
        LOGGER.info("Creating devices...")

        headers = {
            "Content-Type": "application/json",
            "Authorization": "Bearer {0}".format(jwt),
        }

        loads = DojotAPI.divide_loads(n, batch)

        for i, load in enumerate(loads):
            device = {
                "templates": [template_id],
                "attrs": {},
                "label": "CargoContainer_{0}".format(i)
            }

            res = requests.post(
                "{0}/device?count={1}&verbose=false".format(CONFIG['dojot']['url'], load),
                data=json.dumps(device),
                headers=headers
            )

            res.raise_for_status()

        LOGGER.info("... created the devices")

    @staticmethod
    def create_template(jwt: str) -> str:
        """
        Create the default template for test devices.

        Returns the created template ID.
        """
        LOGGER.info("Creating template...")

        template = {
            "label": "CargoContainer",
            "attrs": [
                {
                    "label": "temperature",
                    "type": "dynamic",
                    "value_type": "float"},
                {
                    "label": "humidity",
                    "type": "dynamic",
                    "value_type": "float"},
                {
                    "label": "lightness",
                    "type": "dynamic",
                    "value_type": "float"},
                {
                    "label" : "gps",
                    "type" : "dynamic",
                    "value_type" : "geo:point"}
            ]
        }

        headers = {
            "Content-Type": "application/json",
            "Authorization": "Bearer {0}".format(jwt),
        }

        res = requests.post(
            "{0}/template".format(CONFIG['dojot']['url']),
            data=json.dumps(template),
            headers=headers
        )

        res.raise_for_status()

        LOGGER.info("... created the template")
        return json.loads(res.text)["template"]["id"]

    @staticmethod
    def delete_devices(jwt: str) -> None:
        """
        Delete all devices.
        """
        LOGGER.info("Deleting devices...")

        headers = {
            "Authorization": "Bearer {0}".format(jwt),
        }

        res = requests.delete(
            "{0}/device".format(CONFIG['dojot']['url']),
            headers=headers
        )
        res.raise_for_status()

        LOGGER.info("... deleted devices")

    @staticmethod
    def delete_templates(jwt: str) -> None:
        """
        Delete all templates.
        """
        LOGGER.info("Deleting templates...")

        headers = {
            "Authorization": "Bearer {0}".format(jwt),
        }

        res = requests.delete(
            "{0}/template".format(CONFIG['dojot']['url']),
            headers=headers
        )
        res.raise_for_status()

        LOGGER.info("... deleted templates")

    @staticmethod
    def get_devices(jwt: str) -> list:
        """
        Retrieves the devices from Dojot.

        Parameters:
            jwt: Dojot JWT token

        Returns a list of IDs.
        """
        LOGGER.info("Retrieving devices...")

        headers = {
            "Content-Type": "application/json",
            "Authorization": "Bearer {0}".format(jwt),
        }

        res = requests.get(
            "{0}/device".format(CONFIG['dojot']['url']),
            headers=headers
        )
        res.raise_for_status()

        devices_ids = [device['id'] for device in json.loads(res.text)['devices']]

        LOGGER.info("... retrieved the devices")

        return devices_ids

    @staticmethod
    def divide_loads(n: int, batch: int) -> list:
        """
        Divides `n` in a list with each element being up to `batch`.
        """
        loads = []

        if n > batch:
            iterations = n // batch
            exceeding = n % batch
            # This will create a list with the number `batch` repeated `iterations` times
            # and then `exceeding` at the final
            loads = [batch] * iterations
            if exceeding > 0:
                loads.append(exceeding)

        else:
            loads.append(n)

        return loads
