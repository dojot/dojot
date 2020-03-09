"""
Tests for RedisClient.
"""

import unittest
from unittest.mock import patch, MagicMock

from src.mqtt_locust.redis_client import RedisClient

MOCK_CONFIG = {
    'locust': {
        'redis': {
            'host': 'mockHost',
            'port': 'mockPort',
            'certificates_db': 'mockCertDb',
            'mapped_db': 'mockMapDb',
        },
    },

    'security': {
        'devices_to_renew': 10,
        'devices_to_revoke': 10,
        'renew_devices': True,
        'revoke_devices': True,
    },
}


@patch('src.mqtt_locust.redis_client.logging')
@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.Utils')
@patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
class RedisClientConstructor(unittest.TestCase):
    """
    Tests for RedisClient constructor.
    """
    def test_constructor_success(self, mock_utils, mock_redis, mock_logging):
        """
        Should create a RedisClient instance.
        """
        RedisClient()

        self.assertTrue(mock_redis.Redis.call_count == 2)
        mock_utils.create_logger.assert_called_once()
        mock_utils.create_logger().error.assert_not_called()

    def test_constructor_error(self, mock_utils, mock_redis, mock_logging):
        """
        Should create a RedisClient instance.
        """
        mock_redis.Redis.side_effect = Exception()

        self.assertRaises(Exception, RedisClient)

        mock_redis.Redis.assert_called_once()
        mock_utils.create_logger.assert_called_once()
        mock_utils.create_logger().error.assert_called_once()


@patch('src.mqtt_locust.redis_client.logging')
@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.Utils')
class RedisClientNextDeviceId(unittest.TestCase):
    """
    Tests for next_device_id().
    """
    def test_success(self, mock_utils, mock_redis, mock_logging):
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

    def test_failure(self, mock_utils, mock_redis, mock_logging):
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


@patch('src.mqtt_locust.redis_client.uuid4')
@patch('src.mqtt_locust.redis_client.logging')
@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.Utils')
class RedisClientHasToRevoke(unittest.TestCase):
    """
    Tests for has_to_revoke().
    """
    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_should_revoke(self, mock_utils, mock_redis, mock_logging, mock_uuid):
        """
        Should revoke.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.return_value = 1

        should_revoke = client.has_to_revoke()

        client.mapped.eval.assert_called_once()
        mock_uuid.assert_called_once()
        mock_utils.create_logger().error.assert_not_called()
        self.assertTrue(should_revoke['should_revoke'])

    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_should_not_revoke(self, mock_utils, mock_redis, mock_logging, mock_uuid):
        """
        Should not revoke because the script returned 0.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.return_value = 0

        should_revoke = client.has_to_revoke()

        client.mapped.eval.assert_called_once()
        mock_uuid.assert_not_called()
        mock_utils.create_logger().error.assert_not_called()
        self.assertIsNone(should_revoke)

    def test_should_not_revoke_config(self, mock_utils, mock_redis, mock_logging, mock_uuid):
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
    def test_exception(self, mock_utils, mock_redis, mock_logging, mock_uuid):
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


@patch('src.mqtt_locust.redis_client.uuid4')
@patch('src.mqtt_locust.redis_client.logging')
@patch('src.mqtt_locust.redis_client.redis')
@patch('src.mqtt_locust.redis_client.Utils')
class RedisClientHasToRenew(unittest.TestCase):
    """
    Tests for has_to_renew().
    """
    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_should_renew(self, mock_utils, mock_redis, mock_logging, mock_uuid):
        """
        Should renew.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.return_value = 1

        should_renew = client.has_to_renew()

        mock_uuid.assert_called_once()
        client.mapped.eval.assert_called_once()
        mock_utils.create_logger().error.assert_not_called()
        self.assertTrue(should_renew['should_renew'])

    @patch.dict('src.mqtt_locust.redis_client.CONFIG', MOCK_CONFIG)
    def test_should_not_renew(self, mock_utils, mock_redis, mock_logging, mock_uuid):
        """
        Should not renew because the script returned 0.
        """
        mock_redis.Redis.return_value = MagicMock()
        client = RedisClient()
        client.mapped.eval.return_value = 0

        should_renew = client.has_to_renew()

        client.mapped.eval.assert_called_once()
        mock_uuid.assert_not_called()
        mock_utils.create_logger().error.assert_not_called()
        self.assertIsNone(should_renew)

    def test_should_not_renew_config(self, mock_utils, mock_redis, mock_logging, mock_uuid):
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
    def test_exception(self, mock_utils, mock_redis, mock_logging, mock_uuid):
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
