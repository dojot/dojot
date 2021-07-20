"""
API calls to Dojot.
"""
import json
from typing import Callable, List, Dict
import sys
import requests
import gevent

from src.config import CONFIG
from src.utils import Utils


LOGGER = Utils.create_logger("api")


class APICallError(Exception):
    """
    Error when trying to call Dojot API.
    """


class DojotAPI():
    """
    Utility class with API calls to Dojot.
    """
    @staticmethod
    def get_jwt() -> str:
        """
        Request a JWT token.
        """
        LOGGER.debug("Retrieving JWT...")

        args = {
            "url": "{0}/auth".format(CONFIG['dojot']['url']),
            "data": json.dumps({
                "username": CONFIG['dojot']['user'],
                "passwd": CONFIG['dojot']['passwd'],
            }),
            "headers": {
                "Content-Type": "application/json"
            },
        }

        res = DojotAPI.call_api(requests.post, args)

        LOGGER.debug(".. retrieved JWT")
        return res["jwt"]

    @staticmethod
    def create_devices(jwt: str, template_id: str, total: int, batch: int) -> None:
        """
        Create the devices.

        Parameters:
            jwt: Dojot JWT token
            template_id: template ID to be used by the devices
            n: total number of devices to be created
            batch: number of devices to be created in each iteration
        """
        LOGGER.debug("Creating devices...")

        args = {
            "headers": {
                "Content-Type": "application/json",
                "Authorization": "Bearer {0}".format(jwt),
            },
        }

        loads = DojotAPI.divide_loads(total, batch)

        for i, load in enumerate(loads):
            args["data"] = json.dumps({
                "templates": [template_id],
                "attrs": {},
                "label": "CargoContainer_{0}".format(i)
            })
            args["url"] = "{0}/device?count={1}&verbose=false".format(CONFIG['dojot']['url'], load)

            DojotAPI.call_api(requests.post, args, False)

        LOGGER.debug("... created the devices")

    @staticmethod
    def create_template(jwt: str) -> str:
        """
        Create the default template for test devices.

        Returns the created template ID.
        """
        LOGGER.debug("Creating template...")

        args = {
            "url": "{0}/template".format(CONFIG['dojot']['url']),
            "headers": {
                "Content-Type": "application/json",
                "Authorization": "Bearer {0}".format(jwt),
            },
            "data": json.dumps({
                "label": "CargoContainer",
                "attrs": [
                    {
                        "label": "timestamp",
                        "type": "dynamic",
                        "value_type": "integer"
                    },
                ]
            }),
        }

        res = DojotAPI.call_api(requests.post, args)

        LOGGER.debug("... created the template")
        return res["template"]["id"]

    @staticmethod
    def create_device(jwt: str, template_id: str, label: str) -> str:
        """
        Create a device in Dojot.

        Parameters:
            jwt: JWT authorization.
            template_id: template to be used by the device.
            label: name for the device in Dojot.

        Returns the created device ID.
        """
        LOGGER.debug("Creating template...")

        args = {
            "url": "{0}/device".format(CONFIG['dojot']['url']),
            "headers": {
                "Content-Type": "application/json",
                "Authorization": "Bearer {0}".format(jwt),
            },
            "data": json.dumps({
                "templates": [template_id],
                "attrs": {},
                "label": label,
            }),
        }

        res = DojotAPI.call_api(requests.post, args)

        LOGGER.debug("... created the template")
        return res["devices"][0]["id"]

    @staticmethod
    def delete_devices(jwt: str) -> None:
        """
        Delete all devices.
        """
        LOGGER.debug("Deleting devices...")

        args = {
            "url": "{0}/device".format(CONFIG['dojot']['url']),
            "headers": {
                "Authorization": "Bearer {0}".format(jwt),
            },
        }

        DojotAPI.call_api(requests.delete, args, False)

        LOGGER.debug("... deleted devices")

    @staticmethod
    def delete_templates(jwt: str) -> None:
        """
        Delete all templates.
        """
        LOGGER.debug("Deleting templates...")

        args = {
            "url": "{0}/template".format(CONFIG['dojot']['url']),
            "headers": {
                "Authorization": "Bearer {0}".format(jwt),
            },
        }

        DojotAPI.call_api(requests.delete, args, False)

        LOGGER.debug("... deleted templates")

    @staticmethod
    def get_devices(jwt: str) -> List:
        """
        Retrieves the devices from Dojot.

        Parameters:
            jwt: Dojot JWT token

        Returns a list of IDs.
        """
        LOGGER.debug("Retrieving devices...")

        args = {
            "url": "{0}/device?page_size={1}".format(
                CONFIG['dojot']['url'],
                CONFIG['dojot']['api']['page_size'],
            ),
            "headers": {
                "Content-Type": "application/json",
                "Authorization": "Bearer {0}".format(jwt),
            },
        }

        devices_ids = []

        res = DojotAPI.call_api(requests.get, args)

        for i in range(res['pagination']['total']):
            args['url'] = "{0}/device?idsOnly=true&page_size={1}&page_num={2}".format(
                CONFIG['dojot']['url'],
                CONFIG['dojot']['api']['page_size'],
                i + 1
            )

            res = DojotAPI.call_api(requests.get, args)

            devices_ids.extend(res)

        LOGGER.debug("... retrieved the devices")

        return devices_ids

    @staticmethod
    def generate_certificate(jwt: str, csr: str) -> str:
        """
        Generate the certificates.

        Parameters:
            jwt: Dojot JWT token
            username: dojot username
            passwd: dojot user password
            csr: CSR in PEM format

        Returns the raw certificate.
        """
        args = {
            "url": CONFIG['dojot']['url'] + "/x509/v1/certificates",
            "headers": {
                "content-type": "application/json",
                "Accept": "application/json",
                "Authorization": "Bearer {0}".format(jwt),
            },
            "data": json.dumps({
                "csr": csr
            }),
        }

        res = DojotAPI.call_api(requests.post, args)

        return (res['certificateFingerprint'], res['certificatePem'])

    @staticmethod
    def associate_device_with_certificate(jwt: str, device_id, fingerprint):
        LOGGER.debug("associating a device with cert...")

        req = json.dumps({
            "belongsTo" : {"device" : device_id}
        })
        args = {
            "url": "{0}/x509/v1/certificates/{1}".format(CONFIG['dojot']['url'], fingerprint),
            "headers": {
                "Content-Type": "application/json",
                "Authorization": "Bearer {0}".format(jwt),
            },
            "data": req,
        }

        DojotAPI.call_api(requests.patch, args, False)

        LOGGER.debug("... configured")

    @staticmethod
    def revoke_certificate(jwt: str, fingerprint: str) -> None:
        """
        Revoke a certificate.

        Params:
            jwt: Dojot JWT token
            username: dojot username
            status: status to be set, defaults to 10
        """
        args = {
            "url": CONFIG['dojot']['url'] + "/x509/v1/certificates/"+ fingerprint,
            "headers": {
                "content-type": "application/json",
                "Accept": "application/json",
                "Authorization": "Bearer {0}".format(jwt),
            }
        }

        DojotAPI.call_api(requests.delete, args, False)


    @staticmethod
    def divide_loads(total: int, batch: int) -> List:
        """
        Divides `n` in a list with each element being up to `batch`.
        """
        loads = []

        if total > batch:
            iterations = total // batch
            exceeding = total % batch
            # This will create a list with the number `batch` repeated `iterations` times
            # and then `exceeding` at the final
            loads = [batch] * iterations
            if exceeding > 0:
                loads.append(exceeding)

        else:
            loads.append(total)

        return loads

    @staticmethod
    def call_api(func: Callable[..., requests.Response], args: dict, return_json: bool = True) ->\
        Dict:
        """
        Calls the Dojot API using `func` and `args`.

        Parameters:
            func: function to call Dojot API.
            args: dictionary of arguments to `func`

        Returns the response in a dictionary
        """
        for _ in range(CONFIG['dojot']['api']['retries'] + 1):
            res = None
            try:
                res = func(**args)
                res.raise_for_status()

            except Exception as exception:
                LOGGER.debug(str(exception))
                if res is not None and res.status_code == 429:
                    LOGGER.error("reached maximum number of requisitions to Dojot")
                    sys.exit(1)
                else:
                    LOGGER.error(str(exception))

                gevent.sleep(CONFIG['dojot']['api']['time'])

            else:
                if return_json:
                    return res.json()
                return

        raise APICallError("exceeded the number of retries to {0}".format(args['url']))
