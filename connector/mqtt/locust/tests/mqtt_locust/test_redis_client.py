"""
Tests for RedisClient.
"""

import unittest
import redis
from unittest.mock import patch, MagicMock

from src.mqtt_locust.redis_client import RedisClient


redis.Redis = MagicMock()


MOCK_CONFIG = {
    'locust': {
        'redis': {
            'host': 'mockHost',
            'port': 'mockPort',
            'certificates_db': 'mockCertDb',
            'mapped_db': 'mockMapDb',
            'jwt_expire_time': 10,
        },
    },

    'security': {
        'devices_to_renew': 10,
        'devices_to_revoke': 10,
        'renew_devices': True,
        'revoke_devices': True,
    },
}


@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.Utils')
@patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
class RedisClientConstructor(unittest.TestCase):
    """
    Tests for RedisClient constructor.
    """
    def test_constructor_success(self, mock_utils, mock_redis):
        """
        Should create a RedisClient instance.
        """
        RedisClient()

        self.assertTrue(mock_redis.Redis.call_count == 2)
        mock_utils.create_logger.assert_called_once()
        mock_utils.create_logger().error.assert_not_called()

    def test_constructor_error(self, mock_utils, mock_redis):
        """
        Should create a RedisClient instance.
        """
        mock_redis.Redis.side_effect = Exception()

        self.assertRaises(Exception, RedisClient)

        mock_redis.Redis.assert_called_once()
        mock_utils.create_logger.assert_called_once()
        mock_utils.create_logger().error.assert_called_once()


@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.Utils')
class RedisClientNextDeviceId(unittest.TestCase):
    """
    Tests for next_device_id().
    """
    def test_success(self, _mock_utils, mock_redis):
        """
        Should return a device ID.
        """
        client = RedisClient()

        # The get() function returns 'bytes'
        mock_redis.Redis().get.return_value = b"testDevice"

        device_id = client.next_device_id()

        client.mapped.incr.assert_called_once()
        client.mapped.incr.assert_called_with("device_count")
        client.mapped.get.assert_called_once()
        self.assertEqual(device_id, "testDevice")

    def test_failure(self, mock_utils, mock_redis):
        """
        Should not return a device ID.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.incr.side_effect = Exception()

        self.assertRaises(Exception, client.next_device_id)

        client.mapped.incr.assert_called_once()
        client.mapped.incr.assert_called_with("device_count")
        mock_utils.create_logger().error.assert_called_once()


@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.Utils')
class RedisClientHasToRevoke(unittest.TestCase):
    """
    Tests for has_to_revoke().
    """
    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_should_revoke(self, mock_utils, mock_redis):
        """
        Should revoke.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.return_value = 1
        client.get_device_id = MagicMock(return_value="testID")

        should_revoke = client.has_to_revoke()

        client.mapped.eval.assert_called_once()
        mock_utils.create_logger().error.assert_not_called()
        self.assertIsNotNone(should_revoke)
        self.assertEqual(should_revoke['device_id'], "testID")
        self.assertTrue(should_revoke['should_revoke'])

    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_should_not_revoke(self, mock_utils, mock_redis):
        """
        Should not revoke because the script returned 0.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.return_value = 0

        should_revoke = client.has_to_revoke()

        client.mapped.eval.assert_called_once()
        mock_utils.create_logger().error.assert_not_called()
        self.assertIsNone(should_revoke)

    def test_should_not_revoke_config(self, mock_utils, mock_redis):
        """
        Should not revoke because the config value for renew_devices is False.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        mock_config = {
            'security': {
                'revoke_devices': False
            }
        }
        patch.dict('src.mqtt_locust.redis_client.CONFIG', mock_config)

        should_revoke = client.has_to_revoke()

        client.mapped.eval.assert_not_called()
        mock_utils.create_logger().error.assert_not_called()
        self.assertIsNone(should_revoke)

    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_exception(self, mock_utils, mock_redis):
        """
        Should raise an exception.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.side_effect = Exception()

        should_revoke = client.has_to_revoke()

        client.mapped.eval.assert_called_once()
        mock_utils.create_logger().error.assert_called_once()
        self.assertIsNone(should_revoke)


@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.Utils')
class RedisClientHasToRenew(unittest.TestCase):
    """
    Tests for has_to_renew().
    """
    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_should_renew(self, mock_utils, mock_redis):
        """
        Should renew.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.return_value = 1
        client.get_device_id = MagicMock(return_value="testID")

        should_renew = client.has_to_renew()

        client.mapped.eval.assert_called_once()
        mock_utils.create_logger().error.assert_not_called()
        self.assertIsNotNone(should_renew)
        self.assertEqual(should_renew['device_id'], "testID")
        self.assertTrue(should_renew['should_renew'])

    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_should_not_renew(self, mock_utils, mock_redis):
        """
        Should not renew because the script returned 0.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.return_value = 0

        should_renew = client.has_to_renew()

        client.mapped.eval.assert_called_once()
        mock_utils.create_logger().error.assert_not_called()
        self.assertIsNone(should_renew)

    def test_should_not_renew_config(self, mock_utils, mock_redis):
        """
        Should not renew.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        mock_config = {
            'security': {
                'renew_devices': False
            }
        }
        patch.dict('src.mqtt_locust.redis_client.CONFIG', mock_config)

        should_renew = client.has_to_renew()

        client.mapped.eval.assert_not_called()
        mock_utils.create_logger().error.assert_not_called()
        self.assertIsNone(should_renew)

    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_exception(self, mock_utils, mock_redis):
        """
        Should raise an exception.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.side_effect = Exception()

        should_renew = client.has_to_renew()

        client.mapped.eval.assert_called_once()
        mock_utils.create_logger().error.assert_called_once()
        self.assertIsNone(should_renew)


@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.DojotAPI')
@patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
class RedisClientGetJwt(unittest.TestCase):
    """
    Tests for get_jwt().
    """
    def test_create_new_jwt(self, mock_api, mock_redis):
        """
        Should create a new JWT.
        """
        mock_redis.Redis().get = MagicMock(return_value=None)
        mock_redis.Redis().setex = MagicMock()
        mock_api.get_jwt = MagicMock(return_value="testJWT")

        client = RedisClient()

        jwt = client.get_jwt()

        mock_redis.Redis().get.assert_called_once()
        mock_redis.Redis().get.assert_called_with("jwt")
        mock_api.get_jwt.assert_called_once()
        mock_redis.Redis().setex.assert_called_once()
        mock_redis.Redis().setex.assert_called_with(
            "jwt",
            MOCK_CONFIG['locust']['redis']['jwt_expire_time'],
            "testJWT"
        )
        self.assertEqual(jwt, "testJWT")

    def test_get_jwt_from_db(self, _mock_api, mock_redis):
        """
        Should get the JWT from the database.
        """
        mock_redis.Redis().get = MagicMock(return_value=b"testJWT")

        client = RedisClient()

        jwt = client.get_jwt()

        mock_redis.Redis().get.assert_called_once()
        mock_redis.Redis().get.assert_called_with("jwt")
        self.assertEqual(jwt, "testJWT")


@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.DojotAPI')
class RedisClientGetTemplateId(unittest.TestCase):
    """
    Tests for get_template_id().
    """
    def test_create_new_template(self, mock_api, mock_redis):
        """
        Should create a new template.
        """
        mock_redis.Redis().get = MagicMock(return_value=b"-1")
        mock_redis.Redis().set = MagicMock()
        mock_api.create_template = MagicMock(return_value="1")

        client = RedisClient()

        client.get_jwt = MagicMock(return_value="testJWT")

        template_id = client.get_template_id()

        mock_redis.Redis().get.assert_called_once()
        mock_redis.Redis().get.assert_called_with("template_id")
        mock_api.create_template.assert_called_once()
        mock_api.create_template.assert_called_with("testJWT")
        mock_redis.Redis().set.assert_called_once()
        mock_redis.Redis().set.assert_called_with("template_id", "1")
        self.assertEqual(template_id, "1")

    def test_get_template_id_from_db(self, _mock_api, mock_redis):
        """
        Should get the template ID from the database.
        """
        mock_redis.Redis().get = MagicMock(return_value=b"2")

        client = RedisClient()

        template_id = client.get_template_id()

        mock_redis.Redis().get.assert_called_once()
        mock_redis.Redis().get.assert_called_with("template_id")
        self.assertEqual(template_id, "2")


@patch('src.mqtt_locust.redis_client.uuid4')
@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.DojotAPI')
class RedisClientGetDeviceId(unittest.TestCase):
    """
    Tests for get_device_id().
    """
    @patch.dict('src.mqtt_locust.redis_client.CONFIG', {'dojot': {'env': False}})
    def test_create_virtual_device_id(self, _mock_api, _mock_redis, mock_uuid):
        """
        Should create a new ID for a virtual device.
        """
        mock_uuid.return_value = "test-UUID"

        client = RedisClient()

        device_id = client.get_device_id()

        self.assertEqual(device_id, "testUUID")

    @patch.dict('src.mqtt_locust.redis_client.CONFIG', {'dojot': {'env': True}})
    def test_get_template_id_from_db(self, mock_api, _mock_redis, mock_uuid):
        """
        Should get the template ID from the database.
        """
        mock_api.create_device = MagicMock(return_value="4")
        mock_uuid.return_value = "test-UUID"

        client = RedisClient()

        client.get_template_id = MagicMock(return_value="3")
        client.get_jwt = MagicMock(return_value="testJWT")

        device_id = client.get_device_id()

        client.get_template_id.assert_called_once()
        mock_api.create_device.assert_called_once_with(
            "testJWT",
            "3",
            "CargoContainer_testUUID"
        )
        self.assertEqual(device_id, "4")
