"""
Tests for DojotAPI class.
"""

import unittest
from unittest.mock import patch, MagicMock, PropertyMock

from src.dojot.api import DojotAPI


MOCK_CONFIG = {
    'dojot': {
        'user': 'admin',
        'passwd': 'admin',
        'url': 'dojot_url',
    },
}


@patch('src.dojot.api.json')
@patch('src.dojot.api.requests')
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG)
class TestDojotAPIGetJwt(unittest.TestCase):
    """
    DojotAPI get_jwt() tests.
    """
    def test_successfully_get_jwt(self, mock_requests, mock_json):
        """
        Should successfully get a JWT from Dojot.
        """
        mock_json.loads.return_value = {"jwt": "testJWT"}

        jwt = DojotAPI.get_jwt()

        self.assertEqual(jwt, "testJWT")

    def test_not_successfully_get_jwt(self, mock_requests, mock_json):
        """
        Should not successfully get a JWT from Dojot - rose an exception.
        """
        mock_requests.post.side_effect = Exception("test_exception")
        jwt = None

        with self.assertRaises(Exception) as context:
            jwt = DojotAPI.get_jwt()

        self.assertIsNotNone(context.exception)
        mock_requests.post.assert_called_once()
        mock_json.dumps.assert_called_once()
        mock_json.loads.assert_not_called()
        self.assertIsNone(jwt)


@patch('src.dojot.api.json')
@patch('src.dojot.api.requests')
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG)
class TestDojotAPICreateDevices(unittest.TestCase):
    """
    DojotAPI create_devices() tests.
    """
    def test_successfully_create_devices(self, mock_requests, mock_json):
        """
        Should successfully create the devices.
        """
        divide_loads = DojotAPI.divide_loads
        DojotAPI.divide_loads = MagicMock()
        DojotAPI.divide_loads.return_value = [1]

        DojotAPI.create_devices("testJWT", "0", 1, 1)

        mock_requests.post.assert_called_once()
        mock_json.dumps.assert_called_once()
        mock_requests.post.return_value.raise_for_status.assert_called_once()

        DojotAPI.divide_loads = divide_loads

    def test_not_successfully_create_devices(self, mock_requests, mock_json):
        """
        Should not successfully create the devices - rose an exception.
        """
        divide_loads = DojotAPI.divide_loads
        DojotAPI.divide_loads = MagicMock()
        DojotAPI.divide_loads.return_value = [1]
        mock_requests.post.side_effect = Exception("test_exception")

        with self.assertRaises(Exception) as context:
            DojotAPI.create_devices("testJWT", "0", 1, 1)

        self.assertIsNotNone(context.exception)
        mock_requests.post.assert_called_once()
        mock_json.dumps.assert_called_once()

        DojotAPI.divide_loads = divide_loads


@patch('src.dojot.api.json')
@patch('src.dojot.api.requests')
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG)
class TestDojotAPICreateTemplate(unittest.TestCase):
    """
    DojotAPI create_template() tests.
    """
    def test_successfully_create_template(self, mock_requests, mock_json):
        """
        Should successfully create the template.
        """
        mock_json.loads.return_value = {"template": {"id": 1}}

        template_id = DojotAPI.create_template("testJWT")

        mock_requests.post.assert_called_once()
        mock_json.dumps.assert_called_once()
        mock_requests.post.return_value.raise_for_status.assert_called_once()
        mock_json.loads.assert_called_once()
        self.assertEqual(template_id, 1)

    def test_not_successfully_create_template(self, mock_requests, mock_json):
        """
        Should not successfully create the template - rose an exception.
        """
        mock_requests.post.side_effect = Exception("test_exception")
        template_id = None

        with self.assertRaises(Exception) as context:
            template_id = DojotAPI.create_template("testJWT")

        self.assertIsNotNone(context.exception)
        mock_requests.post.assert_called_once()
        mock_json.dumps.assert_called_once()
        self.assertIsNone(template_id)


@patch('src.dojot.api.json')
@patch('src.dojot.api.requests')
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG)
class TestDojotAPIDeleteDevices(unittest.TestCase):
    """
    DojotAPI delete_devices() tests.
    """
    def test_successfully_delete_devices(self, mock_requests, mock_json):
        """
        Should successfully delete devices.
        """
        mock_json.loads.return_value = {"template": {"id": 1}}

        DojotAPI.delete_devices("testJWT")

        mock_requests.delete.assert_called_once()
        mock_requests.delete.return_value.raise_for_status.assert_called_once()

    def test_not_successfully_delete_devices(self, mock_requests, mock_json):
        """
        Should not successfully delete devices - rose an exception.
        """
        mock_requests.delete.side_effect = Exception("test_exception")

        with self.assertRaises(Exception) as context:
            DojotAPI.delete_devices("testJWT")

        self.assertIsNotNone(context.exception)
        mock_requests.delete.assert_called_once()


@patch('src.dojot.api.json')
@patch('src.dojot.api.requests')
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG)
class TestDojotAPIDeleteTemplates(unittest.TestCase):
    """
    DojotAPI delete_templates() tests.
    """
    def test_successfully_delete_templates(self, mock_requests, mock_json):
        """
        Should successfully delete templates.
        """
        mock_json.loads.return_value = {"template": {"id": 1}}

        DojotAPI.delete_templates("testJWT")

        mock_requests.delete.assert_called_once()
        mock_requests.delete.return_value.raise_for_status.assert_called_once()

    def test_not_successfully_delete_devices(self, mock_requests, mock_json):
        """
        Should not successfully delete templates - rose an exception.
        """
        mock_requests.delete.side_effect = Exception("test_exception")

        with self.assertRaises(Exception) as context:
            DojotAPI.delete_templates("testJWT")

        self.assertIsNotNone(context.exception)
        mock_requests.delete.assert_called_once()


@patch('src.dojot.api.json')
@patch('src.dojot.api.requests')
@patch.dict('src.dojot.api.CONFIG', MOCK_CONFIG)
class TestDojotAPIGetDevices(unittest.TestCase):
    """
    DojotAPI get_devices() tests.
    """
    def test_successfully_get_devices(self, mock_requests, mock_json):
        """
        Should successfully get devices.
        """
        mock_json.loads.return_value = {"devices": [{"id": 0},{"id": 1}]}

        devices = DojotAPI.get_devices("testJWT")

        mock_requests.get.assert_called_once()
        mock_json.loads.assert_called_once()
        mock_requests.get.return_value.raise_for_status.assert_called_once()
        self.assertEqual(devices, [0, 1])

    def test_not_successfully_get_devices(self, mock_requests, mock_json):
        """
        Should not successfully get templates - rose an exception.
        """
        mock_requests.get.side_effect = Exception("test_exception")

        devices = []

        with self.assertRaises(Exception) as context:
            devices = DojotAPI.get_devices("testJWT")

        self.assertIsNotNone(context.exception)
        mock_requests.get.assert_called_once()
        mock_json.loads.assert_not_called()


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
