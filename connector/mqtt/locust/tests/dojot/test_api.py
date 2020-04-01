"""
Tests for DojotAPI class.
"""

import unittest
from unittest.mock import patch, MagicMock, ANY, PropertyMock

import requests

from src.dojot.api import APICallError, DojotAPI


MOCK_CONFIG = {
    'dojot': {
        'user': 'admin',
        'passwd': 'admin',
        'url': 'dojot_url',
        'api': {
            'page_size': 1,
        }
    },
}


@patch('src.dojot.api.json', autospec=True)
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

    def test_successfully_get_jwt(self, mock_requests, mock_json):
        """
        Should successfully get a JWT from Dojot.
        """
        DojotAPI.call_api.return_value = {"jwt": "testJWT"}

        jwt = DojotAPI.get_jwt()

        mock_json.dumps.assert_called_once()
        DojotAPI.call_api.assert_called_once()
        DojotAPI.call_api.assert_called_with(mock_requests.post, ANY)
        self.assertEqual(jwt, "testJWT")


@patch('src.dojot.api.json', autospec=True)
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

    def tearDown(self):
        DojotAPI.call_api = self.call_api
        DojotAPI.divide_loads = self.divide_loads

    def test_successfully_create_one_device_one_batch(self, mock_requests, mock_json):
        """
        Should successfully create one device in one batch.
        """
        DojotAPI.divide_loads.return_value = [1]

        DojotAPI.create_devices("testJWT", "0", 1, 1)

        DojotAPI.divide_loads.assert_called_once()
        DojotAPI.divide_loads.assert_called_with(1, 1)
        self.assertEqual(DojotAPI.divide_loads.return_value, [1])
        mock_json.dumps.assert_called_once()
        DojotAPI.call_api.assert_called_once()
        DojotAPI.call_api.assert_called_with(mock_requests.post, ANY)

    def test_successfully_create_two_devices_two_batches(self, mock_requests, mock_json):
        """
        Should successfully create two devices in two batches.
        """
        DojotAPI.divide_loads.return_value = [1, 1]

        DojotAPI.create_devices("testJWT", "0", 2, 2)

        DojotAPI.divide_loads.assert_called_once()
        DojotAPI.divide_loads.assert_called_with(2, 2)
        self.assertEqual(DojotAPI.divide_loads.return_value, [1, 1])
        self.assertEqual(mock_json.dumps.call_count, 2)
        self.assertEqual(DojotAPI.call_api.call_count, 2)
        for call in DojotAPI.call_api.mock_calls:
            call.assert_called_with(mock_requests.post, ANY)


@patch('src.dojot.api.json', autospec=True)
@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPICreateTemplate(unittest.TestCase):
    """
    DojotAPI create_template() tests.
    """
    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_successfully_create_template(self, mock_requests, mock_json):
        """
        Should successfully create the template.
        """
        DojotAPI.call_api.return_value = {"template": {"id": 1}}

        template_id = DojotAPI.create_template("testJWT")

        mock_json.dumps.assert_called_once()
        DojotAPI.call_api.assert_called_once()
        DojotAPI.call_api.assert_called_with(mock_requests.post, ANY)
        self.assertEqual(template_id, 1)


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPIDeleteDevices(unittest.TestCase):
    """
    DojotAPI delete_devices() tests.
    """
    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_successfully_delete_devices(self, mock_requests):
        """
        Should successfully delete devices.
        """
        DojotAPI.delete_devices("testJWT")

        DojotAPI.call_api.assert_called_once()
        DojotAPI.call_api.assert_called_with(mock_requests.delete, ANY)


@patch('src.dojot.api.requests', autospec=True)
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG, autospec=True)
class TestDojotAPIDeleteTemplates(unittest.TestCase):
    """
    DojotAPI delete_templates() tests.
    """
    def setUp(self):
        self.call_api = DojotAPI.call_api
        DojotAPI.call_api = MagicMock()

    def tearDown(self):
        DojotAPI.call_api = self.call_api

    def test_successfully_delete_templates(self, mock_requests):
        """
        Should successfully delete templates.
        """
        DojotAPI.delete_templates("testJWT")

        DojotAPI.call_api.assert_called_once()
        DojotAPI.call_api.assert_called_with(mock_requests.delete, ANY)


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
@patch.dict(
    'src.dojot.api.CONFIG',
    {'dojot': {'api': {'retries': 1, 'time': 5.0}}},
    autospec=True
)
class TestDojotAPICallApi(unittest.TestCase):
    """
    Tests for call_api().
    """
    def test_should_get_response(self, mock_requests, mock_gevent):
        """
        Should successfully receive a response from a POST call.
        """
        mock_requests.post = MagicMock()
        mock_requests.post.return_value.json = PropertyMock(return_value={"return": "testReturn"})

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

    def test_should_not_get_response(self, mock_requests, mock_gevent):
        """
        Should not receive a response from a POST call - exceptions rose.
        """
        mock_requests.post = MagicMock()
        mock_requests.post.return_value.raise_for_status = MagicMock(side_effect=Exception())

        args = {
            "url": "testURL",
            "data": "\"testJson\": \"testData\"",
            "headers": "testHeader"
        }

        res = None
        with self.assertRaises(APICallError) as context:
            res = DojotAPI.call_api(mock_requests.post, args)

        self.assertIsNotNone(context.exception)
        self.assertIsInstance(type(context.exception), type(APICallError))
        self.assertEqual(mock_requests.post.call_count, 2)
        for call in mock_requests.post.mock_calls:
            call.assert_called_with(**args)
        self.assertEqual(mock_gevent.sleep.call_count, 2)
        for call in mock_gevent.sleep.mock_calls:
            call.assert_called_with(5.0)
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
        self.assertIsInstance(type(context.exception), type(SystemExit))
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
        mock_requests.post.return_value.json = PropertyMock(return_value={"return": "testReturn"})

        args = {
            "url": "testURL",
            "data": "\"testJson\": \"testData\"",
            "headers": "testHeader"
        }

        res = DojotAPI.call_api(mock_requests.post, args)

        self.assertEqual(mock_requests.post.call_count, 2)
        for call in mock_requests.post.mock_calls:
            call.assert_called_with(**args)
        mock_gevent.sleep.assert_called_once_with(5.0)
        self.assertEqual(res, {"return": "testReturn"})
