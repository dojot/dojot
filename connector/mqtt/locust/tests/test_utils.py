"""
  Contains test for Utils module
"""

import sys
import unittest
from unittest.mock import patch
from logging import DEBUG, WARNING, ERROR, CRITICAL, INFO
import random
import paho.mqtt.client as mqtt
from src.utils import Utils


class TestUtils(unittest.TestCase):
    """
      Test Utils Testcase
    """
    def test_str_to_bool_true(self):
        """
            str_to_bool("True") should return boolean True.
        """
        self.assertTrue(Utils.str_to_bool("True"))

    def test_str_to_bool_false(self):
        """
        str_to_bool("False") should return boolean False.
        """
        self.assertFalse(Utils.str_to_bool("False"))

    def test_str_to_bool_other_value(self):
        """
        str_to_bool("other") should return boolean False.
        """
        self.assertFalse(Utils.str_to_bool("other"))

    def test_error_message(self):
        """
        error_message() should return the correct names for errors.
        """
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_AGAIN), "MQTT_ERR_AGAIN")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_SUCCESS), "MQTT_ERR_SUCCESS")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_NOMEM), "MQTT_ERR_NOMEM")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_PROTOCOL), "MQTT_ERR_PROTOCOL")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_INVAL), "MQTT_ERR_INVAL")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_NO_CONN), "MQTT_ERR_NO_CONN")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_CONN_REFUSED), "MQTT_ERR_CONN_REFUSED")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_NOT_FOUND), "MQTT_ERR_NOT_FOUND")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_CONN_LOST), "MQTT_ERR_CONN_LOST")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_TLS), "MQTT_ERR_TLS")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_PAYLOAD_SIZE), "MQTT_ERR_PAYLOAD_SIZE")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_NOT_SUPPORTED), "MQTT_ERR_NOT_SUPPORTED")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_AUTH), "MQTT_ERR_AUTH")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_ACL_DENIED), "MQTT_ERR_ACL_DENIED")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_UNKNOWN), "MQTT_ERR_UNKNOWN")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_ERRNO), "MQTT_ERR_ERRNO")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_QUEUE_SIZE), "MQTT_ERR_QUEUE_SIZE")
        self.assertEqual(Utils.error_message(
            101010), "101010\n")

    def test_log_level(self):
        """"
        log_level() should return correct log level
        """
        self.assertEqual(Utils.log_level('debug'), DEBUG)
        self.assertEqual(Utils.log_level('info'), INFO)
        self.assertEqual(Utils.log_level('warning'), WARNING)
        self.assertEqual(Utils.log_level('error'), ERROR)
        self.assertEqual(Utils.log_level('critical'), CRITICAL)
        self.assertEqual(Utils.log_level('not-exist'), INFO)

    def test_validate_device_id(self):
        """"
        test_validate_device_id(dev-id) raised an exception when (dev-id) < 1
        """
        # not raise
        device_id = "123abc"
        Utils.validate_device_id(device_id)

        with self.assertRaises(ValueError):
            Utils.validate_device_id('')

    def test_validate_thing_id(self):
        """"
        test_validate_thing_id(thing-id) raised an exception when thing id is not correct
        """
        # not raise
        thing_id = "tenant:devid"
        Utils.validate_thing_id(thing_id)

        with self.assertRaises(ValueError):
            Utils.validate_thing_id('')

    def test_create_thing_id(self):
        """"
        test_create_thing_id() should create a thing id
        """
        self.assertEqual(Utils.create_thing_id(
            'tenant', 'dev-id'), 'tenant:dev-id')
        with self.assertRaises(ValueError):
            self.assertEqual(Utils.create_thing_id('tenant', ''), None)
            self.assertEqual(Utils.create_thing_id('', 'dev-id'), None)
            self.assertEqual(Utils.create_thing_id('', ''), None)

    def test_validate_tenant(self):
        """"
        test_validate_tenant(dev-id) raised an exception when (tenant) < 1
        """
        # not raise
        tenant = "123abc"
        Utils.validate_tenant(tenant)

        with self.assertRaises(ValueError):
            Utils.validate_tenant('')

    def test_is_master(self):
        """
        is_master() should return True when the program is run with --master flag
        """
        master_flag = ["--master"]
        with patch.object(sys, "argv", master_flag):
            self.assertTrue(Utils.is_master())

    def test_is_slave(self):
        """
        is_slave() should return True when the program is run with --slave flag
        """
        slave_flag = ["--slave"]
        with patch.object(sys, "argv", slave_flag):
            self.assertTrue(Utils.is_slave())

    @staticmethod
    @patch('src.utils.events')
    def test_fire_locust_failure(mock_events):
        """
        fire_locust_failure() should fire a Locust event on failure
        """
        Utils.fire_locust_failure()
        mock_events.request_failure.fire.assert_called_once()
        mock_events.reset_mock()

    @staticmethod
    @patch('src.utils.events')
    def test_fire_locust_success(mock_events):
        """
        fire_locust_success() should fire a Locust event on success
        """
        Utils.fire_locust_success()
        mock_events.request_success.fire.assert_called_once()
        mock_events.reset_mock()

    def test_seconds_to_milliseconds(self):
        """
        seconds_to_milliseconds() should return the correct value.
        """
        res = Utils.seconds_to_milliseconds(1.0)
        self.assertTrue(res == 1000)

        res = Utils.seconds_to_milliseconds(1.5)
        self.assertTrue(res == 1500)

        res = Utils.seconds_to_milliseconds(1)
        self.assertTrue(res == 1000)

    def test_should_execute(self):
        """
        should_execute() should return the correct value.
        """
        random.seed(42)
        val = random.random()

        random.seed(42)
        self.assertFalse(Utils.should_execute(val - 0.01))

        random.seed(42)
        self.assertFalse(Utils.should_execute(val))

        random.seed(42)
        self.assertTrue(Utils.should_execute(val + 0.01))

    @patch('logging.getLogger')
    @patch('logging.StreamHandler')
    @patch('logging.Formatter')
    @patch('os.environ.get')
    def test_create_logger(
            self,
            mock_os_env_get,
            mock_formatter,
            mock_stream_handler,
            mock_get_logger):
        """
        create_logger() should create a logger with the supplied name.
        """
        mock_os_env_get.get.return_value = "info"

        logger_name = "test_logger"
        logger = Utils.create_logger(name=logger_name)

        mock_get_logger.assert_called_with(logger_name)
        mock_stream_handler.assert_called_once()
        mock_formatter.assert_called_once()

        self.assertTrue(logger.hasHandlers())

        mock_os_env_get.reset_mock()
        mock_formatter.reset_mock()
        mock_stream_handler.reset_mock()
        mock_get_logger.reset_mock()

    def test_get_metric_connected_index_by_time(self):
        self.assertEqual(Utils.get_metric_connected_index_by_time(100),1)
        self.assertEqual(Utils.get_metric_connected_index_by_time(301),2)
        self.assertEqual(Utils.get_metric_connected_index_by_time(550),3)
        self.assertEqual(Utils.get_metric_connected_index_by_time(701),4)
        self.assertEqual(Utils.get_metric_connected_index_by_time(810),5)
        self.assertEqual(Utils.get_metric_connected_index_by_time(1005),6)
        self.assertEqual(Utils.get_metric_connected_index_by_time(1201),7)
        self.assertEqual(Utils.get_metric_connected_index_by_time(1403),8)
        self.assertEqual(Utils.get_metric_connected_index_by_time(1603),9)
        self.assertEqual(Utils.get_metric_connected_index_by_time(1801),10)
        self.assertEqual(Utils.get_metric_connected_index_by_time(2503),11)
        self.assertEqual(Utils.get_metric_connected_index_by_time(40043),11)

    def test_get_metric_disconnected_index_by_time(self):
        self.assertEqual(Utils.get_metric_disconnected_index_by_time(1),1)
        self.assertEqual(Utils.get_metric_disconnected_index_by_time(8),2)
        self.assertEqual(Utils.get_metric_disconnected_index_by_time(14),3)
        self.assertEqual(Utils.get_metric_disconnected_index_by_time(19),4)
        self.assertEqual(Utils.get_metric_disconnected_index_by_time(21),5)
        self.assertEqual(Utils.get_metric_disconnected_index_by_time(29),6)
        self.assertEqual(Utils.get_metric_disconnected_index_by_time(100),7)

if __name__ == "__main__":
    unittest.main()
