"""
Tests for DojotAPI class.
"""

import unittest
from unittest.mock import patch, MagicMock, ANY, PropertyMock
from unittest import mock

import json
import requests

from src.dojot.api import APICallError, DojotAPI


MOCK_CONFIG = {
    'app': {
        'tenant': 'admin'
    },
    'dojot': {
        'user': 'admin',
        'passwd': 'admin',
        'url': 'dojot_url',
        'api': {
            'page_size': 1,
            'retries': 1,
            'time': 5.0
        }
    },
}


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPIGetJwt(unittest.TestCase):
    """
    DojotAPI get_jwt() tests.
    """

    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_successfully_get_jwt(self, mock_requests):
        """
        Should successfully get a JWT from Dojot.
        """
        args = {
            "url": "{0}/auth/realms/{1}/protocol/openid-connect/token".format(MOCK_CONFIG['dojot']['url'], MOCK_CONFIG['app']['tenant']),
            "data": {
                "username": MOCK_CONFIG['dojot']['user'],
                "password": MOCK_CONFIG['dojot']['passwd'],
                "client_id": "admin-cli",
                "grant_type": "password",
            }
        }
        DojotAPI.call_api.return_value = {"access_token": "testJWT"}

        jwt = DojotAPI.get_jwt()

        DojotAPI.call_api.assert_called_once()
        DojotAPI.call_api.assert_called_with(mock_requests.post, args)
        self.assertEqual(jwt, "testJWT")


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPICreateDevices(unittest.TestCase):
    """
    DojotAPI create_devices() tests.
    """

    def setUp(self):
        self.call_api = DojotAPI.call_api
        self.divide_loads = DojotAPI.divide_loads

        DojotAPI.call_api = MagicMock()
        DojotAPI.divide_loads = MagicMock()

        self.jwt = "testJWT"
        self.template_id = "0"
        self.args = {
            "headers": {
                "Content-Type": "application/json",
                "Authorization": "Bearer {0}".format(self.jwt),
            },
            "data": {},
            "url": "{0}/device?count=1&verbose=false".format(MOCK_CONFIG['dojot']['url'])
        }

    def tearDown(self):
        DojotAPI.call_api = self.call_api
        DojotAPI.divide_loads = self.divide_loads

    def test_successfully_create_one_device_one_batch(self, mock_requests):
        """
        Should successfully create one device in one batch.
        """
        DojotAPI.divide_loads.return_value = [1]

        self.args["data"] = json.dumps({
            "templates": [self.template_id],
            "attrs": {},
            "label": "CargoContainer_0"
        })

        DojotAPI.create_devices(self.jwt, self.template_id, 1, 1)

        DojotAPI.divide_loads.assert_called_once_with(1, 1)
        self.assertEqual(DojotAPI.divide_loads.return_value, [1])
        DojotAPI.call_api.assert_called_once_with(
            mock_requests.post, self.args, False)

    def test_successfully_create_two_devices_two_batches(self, mock_requests):
        """
        Should successfully create two devices in two batches.
        """
        DojotAPI.divide_loads.return_value = [1, 1]

        self.args["data"] = json.dumps({
            "templates": [self.template_id],
            "attrs": {},
            "label": "CargoContainer_1"
        })

        DojotAPI.create_devices(self.jwt, self.template_id, 2, 2)

        DojotAPI.divide_loads.assert_called_once_with(2, 2)
        self.assertEqual(DojotAPI.divide_loads.return_value, [1, 1])
        self.assertEqual(DojotAPI.call_api.call_count, 2)
        DojotAPI.call_api.assert_has_calls([
            mock.call(mock_requests.post, self.args, False),
            mock.call(mock_requests.post, self.args, False)
        ])


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPICreateTemplate(unittest.TestCase):
    """
    DojotAPI create_template() tests.
    """

    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

        self.jwt = "testJWT"

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_successfully_create_template(self, mock_requests):
        """
        Should successfully create the template.
        """
        DojotAPI.call_api.return_value = {"template": {"id": 1}}

        args = {
            "url": "{0}/template".format(MOCK_CONFIG['dojot']['url']),
            "headers": {
                "Content-Type": "application/json",
                "Authorization": "Bearer {0}".format(self.jwt),
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

        template_id = DojotAPI.create_template(self.jwt)

        DojotAPI.call_api.assert_called_once_with(mock_requests.post, args)
        self.assertEqual(template_id, 1)


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPICreateDevice(unittest.TestCase):
    """
    DojotAPI create_device() tests.
    """

    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

        self.jwt = "testJWT"
        self.template_id = "0"
        self.label = "testLabel"

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_successfully_create_device(self, mock_requests):
        """
        Should successfully create the device.
        """
        DojotAPI.call_api.return_value = {"devices": [{"id": 1}]}

        args = {
            "url": "{0}/device".format(MOCK_CONFIG['dojot']['url']),
            "headers": {
                "Content-Type": "application/json",
                "Authorization": "Bearer {0}".format(self.jwt),
            },
            "data": json.dumps({
                "templates": [self.template_id],
                "attrs": {},
                "label": self.label,
            }),
        }

        device_id = DojotAPI.create_device(
            self.jwt, self.template_id, self.label)

        DojotAPI.call_api.assert_called_once_with(mock_requests.post, args)
        self.assertEqual(device_id, 1)


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPIDeleteDevices(unittest.TestCase):
    """
    DojotAPI delete_devices() tests.
    """

    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

        self.jwt = "testJWT"

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_successfully_delete_devices(self, mock_requests):
        """
        Should successfully delete devices.
        """
        args = {
            "url": "{0}/device".format(MOCK_CONFIG['dojot']['url']),
            "headers": {
                "Authorization": "Bearer {0}".format(self.jwt),
            },
        }

        DojotAPI.delete_devices(self.jwt)

        DojotAPI.call_api.assert_called_once()
        DojotAPI.call_api.assert_called_with(mock_requests.delete, args, False)


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPIDeleteTemplates(unittest.TestCase):
    """
    DojotAPI delete_templates() tests.
    """

    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

        self.jwt = "testJWT"

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_successfully_delete_templates(self, mock_requests):
        """
        Should successfully delete templates.
        """
        args = {
            "url": "{0}/template".format(MOCK_CONFIG['dojot']['url']),
            "headers": {
                "Authorization": "Bearer {0}".format(self.jwt),
            },
        }

        DojotAPI.delete_templates(self.jwt)

        DojotAPI.call_api.assert_called_once()
        DojotAPI.call_api.assert_called_with(mock_requests.delete, args, False)


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPIGetDevices(unittest.TestCase):
    """
    DojotAPI get_devices() tests.
    """

    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_successfully_get_devices(self, mock_requests):
        """
        Should successfully get devices.
        """
        DojotAPI.call_api.side_effect = [{
            "devices": [{"id": 0}, {"id": 1}],
            "pagination": {
                "total": 2
            }
        }, [0], [1]]

        devices = DojotAPI.get_devices("testJWT")

        self.assertEqual(DojotAPI.call_api.call_count, 3)
        DojotAPI.call_api.assert_called_with(mock_requests.get, ANY)
        self.assertEqual(devices, [0, 1])

    def test_successfully_no_devices(self, mock_requests):
        """
        Should successfully get an empty list of devices.
        """
        DojotAPI.call_api.return_value = {
            "devices": [],
            "pagination": {
                "total": 0
            }
        }

        devices = DojotAPI.get_devices("testJWT")

        DojotAPI.call_api.assert_called_once()
        DojotAPI.call_api.assert_called_with(mock_requests.get, ANY)
        self.assertEqual(devices, [])


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPIGenerateCertificate(unittest.TestCase):
    """
    DojotAPI generate_certificate() tests.
    """

    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

        self.jwt = "testJWT"
        self.username = "testUser"
        self.passwd = "testPasswd"
        self.csr = "testCsr"
        self.args = {
            "url": MOCK_CONFIG['dojot']['url'] + "/x509/v1/certificates",
            "headers": {
                "content-type": "application/json",
                "Accept": "application/json",
                "Authorization": "Bearer {0}".format(self.jwt),
            },
            "data": json.dumps({
                "csr": self.csr
            }),
        }

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_generate_certificate(self, mock_requests):
        """
        Test generate certificate
        """
        DojotAPI.generate_certificate(self.jwt, self.csr)

        DojotAPI.call_api.assert_called_once_with(
            mock_requests.post, self.args)

    def test_generate_certificate_exception(self, mock_requests):
        """
        Should not generate the cert, because rose an exception.
        """
        DojotAPI.call_api.side_effect = APICallError()

        with self.assertRaises(Exception) as context:
            DojotAPI.generate_certificate(self.jwt, self.csr)

        self.assertIsNotNone(context.exception)
        self.assertIsInstance(context.exception, APICallError)

        DojotAPI.call_api.assert_called_once_with(
            mock_requests.post, self.args)


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPIRevokeCertificate(unittest.TestCase):
    """
    DojotAPI revoke_certificate() tests.
    """

    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

        self.jwt = "testJWT"
        self.crt = {}
        self.crt['fingerprint'] = "testFingerprint"
        self.args = {
            "url": MOCK_CONFIG['dojot']['url'] + "/x509/v1/certificates/" + self.crt['fingerprint'],
            "headers": {
                "content-type": "application/json",
                "Accept": "application/json",
                "Authorization": "Bearer {0}".format(self.jwt),
            }
        }

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_revoke_certificate(self, mock_requests):
        """
        Should revoke the certificate correctly.
        """

        DojotAPI.revoke_certificate(self.jwt, self.crt['fingerprint'])

        DojotAPI.call_api.assert_called_once_with(
            mock_requests.delete, self.args, False)

    def test_revoke_certificate_exception(self, mock_requests):
        """
        Should not generate cert, because rose an exception.
        """
        DojotAPI.call_api.side_effect = APICallError()

        with self.assertRaises(Exception) as context:
            DojotAPI.revoke_certificate(self.jwt, self.crt['fingerprint'])

        self.assertIsNotNone(context.exception)
        self.assertIsInstance(context.exception, APICallError)

        DojotAPI.call_api.assert_called_once_with(
            mock_requests.delete, self.args, False)


class TestDojotAPIDivideLoads(unittest.TestCase):
    """
    DojotAPI divide_loads() tests.
    """

    def test_successfully_divide_loads_even(self):
        """
        Should successfully divide the load - even case (all loads are the same).
        """
        loads = DojotAPI.divide_loads(4, 2)

        self.assertEqual(loads, [2, 2])

    def test_successfully_divide_loads_odd(self):
        """
        Should successfully divide the load - odd case (last load is different).
        """
        loads = DojotAPI.divide_loads(5, 2)

        self.assertEqual(loads, [2, 2, 1])

    def test_successfully_divide_loads_low_n(self):
        """
        Should successfully divide the load - n < batch.
        """
        loads = DojotAPI.divide_loads(2, 4)

        self.assertEqual(loads, [2])


@patch('src.dojot.api.gevent', autospec=True)
@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPICallApi(unittest.TestCase):
    """
    Tests for call_api().
    """

    def test_should_get_response(self, mock_requests, mock_gevent):
        """
        Should successfully receive a response from a POST call.
        """
        mock_requests.post = MagicMock()
        mock_requests.post.return_value.json = PropertyMock(
            return_value={"return": "testReturn"})

        args = {
            "url": "testURL",
            "data": "\"testJson\": \"testData\"",
            "headers": "testHeader"
        }

        res = DojotAPI.call_api(mock_requests.post, args)

        mock_requests.post.assert_called_once_with(**args)
        mock_gevent.sleep.assert_not_called()
        mock_requests.post.return_value.json.assert_called_once()
        self.assertEqual(res, {"return": "testReturn"})

    def test_should_not_return_json(self, mock_requests, mock_gevent):
        """
        Should successfully make a POST call but do not return a JSON.
        """
        mock_requests.post = MagicMock()
        mock_requests.post.return_value.json = PropertyMock(
            return_value={"return": "testReturn"})

        args = {
            "url": "testURL",
            "data": "\"testJson\": \"testData\"",
            "headers": "testHeader"
        }

        res = DojotAPI.call_api(mock_requests.post, args, False)

        mock_requests.post.assert_called_once_with(**args)
        mock_gevent.sleep.assert_not_called()
        mock_requests.post.return_value.json.assert_not_called()
        self.assertIsNone(res)

    def test_should_not_get_response(self, mock_requests, mock_gevent):
        """
        Should not receive a response from a POST call - exceptions rose.
        """
        mock_requests.post = MagicMock()
        mock_requests.post.return_value.raise_for_status = MagicMock(
            side_effect=Exception())

        args = {
            "url": "testURL",
            "data": "\"testJson\": \"testData\"",
            "headers": "testHeader"
        }

        res = None
        with self.assertRaises(APICallError) as context:
            res = DojotAPI.call_api(mock_requests.post, args)

        self.assertIsNotNone(context.exception)
        self.assertIsInstance(context.exception, APICallError)

        self.assertEqual(mock_requests.post.call_count, 2)
        mock_requests.post.assert_has_calls([
            mock.call(**args),
            mock.call(**args),
        ], any_order=True)

        self.assertEqual(mock_gevent.sleep.call_count, 2)
        mock_gevent.sleep.assert_has_calls([mock.call(5.0), mock.call(5.0)])

        self.assertIsNone(res)

    def test_should_not_get_response_limit_calls(self, mock_requests, mock_gevent):
        """
        Should not receive a response from a POST call - limit of calls to the API.
        """
        mock_requests.post = MagicMock()
        mock_requests.post.return_value.status_code = 429
        mock_requests.post.return_value.raise_for_status = \
            MagicMock(side_effect=requests.exceptions.HTTPError())

        args = {
            "url": "testURL",
            "data": "\"testJson\": \"testData\"",
            "headers": "testHeader"
        }

        res = None
        with self.assertRaises(SystemExit) as context:
            res = DojotAPI.call_api(mock_requests.post, args)

        self.assertIsNotNone(context.exception)
        self.assertIsInstance(context.exception, SystemExit)

        mock_requests.post.assert_called_once_with(**args)
        mock_gevent.sleep.assert_not_called()

        self.assertIsNone(res)

    def test_should_get_response_after_retry(self, mock_requests, mock_gevent):
        """
        Should successfully receive a response from a POST call in the second call.
        """
        mock_requests.post = MagicMock()
        mock_requests.post.return_value.raise_for_status = MagicMock(
            side_effect=[Exception(), None]
        )
        mock_requests.post.return_value.json = PropertyMock(
            return_value={"return": "testReturn"})

        args = {
            "url": "testURL",
            "data": "\"testJson\": \"testData\"",
            "headers": "testHeader"
        }

        res = DojotAPI.call_api(mock_requests.post, args)

        self.assertEqual(mock_requests.post.call_count, 2)
        mock_requests.post.assert_has_calls([
            mock.call(**args),
            mock.call(**args),
        ], any_order=True)

        mock_gevent.sleep.assert_called_once_with(5.0)

        self.assertEqual(res, {"return": "testReturn"})
