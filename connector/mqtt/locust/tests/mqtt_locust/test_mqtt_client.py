"""
Tests for the MQTT client class.
"""

import unittest
from unittest.mock import patch, MagicMock
import paho.mqtt.client as mqtt
from src.mqtt_locust.mqtt_client import MQTTClient


mqtt.Client = MagicMock()


MOCK_CONFIG = {
    'app': {
        'tenant': 'tenant',
    },
    'security': {
        'cert_dir': 'cert-dir',
        'revoke_cert_dir': 'revoke-cert-dir',
        'renew_cert_dir': 'renew-cert-dir',
        'ca_cert_file': 'ca-cert-file',
        'min_time_reconn': 0,
        'max_time_reconn': 1,
        'time_to_renew': 100,
        'time_to_revoke': 100,
    },
    'mqtt': {
        'host': 'host',
        'port': 0,
        'con_timeout': 0,
        'qos': 0
    }
}


@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientConstructor(unittest.TestCase):
    """
    MQTTClient constructor() unit tests.
    """
    def test_constructor_success(self, mock_utils, _mock_paho):
        """
        Should create a MQTTClient instance.
        """
        device_id = "123"
        run_id = "987"
        tenant = MOCK_CONFIG['app']['tenant']

        client = MQTTClient(device_id, run_id, False, False)
        mock_utils.validate_tenant.assert_called_once_with(tenant)
        mock_utils.validate_device_id.assert_called_once_with(device_id)

        self.assertEqual(client.device_id, device_id)
        self.assertEqual(client.run_id, run_id)
        self.assertEqual(client.should_revoke, False)
        self.assertEqual(client.should_renew, False)

        self.assertFalse(client.is_connected)
        self.assertEqual(client.disconnect_forever, False)
        self.assertIsNotNone(client.mqttc)

        self.assertEqual(client.tenant, tenant)
        self.assertIsNotNone(client.username)
        self.assertIsNotNone(client.topic)
        self.assertIsNotNone(client.sub_topic)

        self.assertEqual(client.device_cert_dir, MOCK_CONFIG["security"]["cert_dir"])
        self.assertIsNone(client.new_cert)

        self.assertEqual(client.pubmmap, {})
        self.assertEqual(client.submmap, {})

        self.assertEqual(client.start_time, 0)

    def test_constructor_invalid_run_id(self, _mock_utils, _mock_paho):
        """
        Should raise a ValueError when passing an invalid run_id.
        """
        with self.assertRaises(ValueError):
            MQTTClient("123", "", False, False)

    def test_constructor_both_true(self, _mock_utils, _mock_paho):
        """
        Should raise a ValueError when passing True to both should_revoke
        and should_renew.
        """
        with self.assertRaises(ValueError):
            MQTTClient("123", "987", True, True)


@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.CertUtils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientCreateCertificate(unittest.TestCase):
    """
    MQTTClient create_certificate() unit tests.
    """

    def test_create_certificate_renew(self, mock_cert_utils, _mock_paho):
        """
        create_certificate() should create the certificates for renewal
        """
        MQTTClient("123", "987", True, False)

        mock_cert_utils.new_cert.assert_called_once()
        mock_cert_utils.create_cert_files.assert_called_once()

    def test_create_certificate_revoke(self, mock_cert_utils, _mock_paho):
        """
        create_certificate() should create the certificates for revoking
        """
        MQTTClient("123", "987", False, True)

        mock_cert_utils.new_cert.assert_called_once()
        mock_cert_utils.create_cert_files.assert_called_once()

    def test_create_certificate_none(self, mock_cert_utils, _mock_paho):
        """
        create_certificate() should create the certificates for revoking/renewal
        """
        MQTTClient("123", "987", False, False)

        self.assertEqual(mock_cert_utils.new_cert.call_count, 0)
        self.assertEqual(mock_cert_utils.create_cert_files.call_count, 0)


@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.CertUtils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientConfigureMqtt(unittest.TestCase):
    """
    MQTTClient configure_mqtt() unit tests.
    """

    def test_configure_mqtt(self, mock_cert_utils, mock_paho):
        """
        Should configure the MQTT connection.
        """
        client = MQTTClient("123", "987", False, True)

        # configure_mqtt() function is called at the constructor, so
        # we need to reset the mocks to be able to test it
        mock_cert_utils.reset_mock()
        mock_paho.reset_mock()

        client.configure_mqtt()

        mock_cert_utils.get_certificate_file.assert_called_once_with(client.device_id)
        mock_cert_utils.get_private_key_file.assert_called_once_with(client.device_id)

        self.assertIsNotNone(client.mqttc)
        mock_paho.Client.assert_called_once_with(client_id=client.device_id)

        client.mqttc.reconnect_delay_set.assert_called_once_with(
            min_delay=MOCK_CONFIG["security"]["min_time_reconn"],
            max_delay=MOCK_CONFIG["security"]["max_time_reconn"]
        )

        client.mqttc.tls_set.assert_called_once()
        client.mqttc.tls_insecure_set.assert_called_once_with(True)

        self.assertIsNotNone(client.mqttc.on_connect)
        self.assertIsNotNone(client.mqttc.on_disconnect)
        self.assertIsNotNone(client.mqttc.on_publish)
        self.assertIsNotNone(client.mqttc.on_subscribe)
        self.assertIsNotNone(client.mqttc.on_message)


@patch('src.mqtt_locust.mqtt_client.Utils')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientConnect(unittest.TestCase):
    """
    MQTTClient connect() unit tests.
    """
    def test_connect(self, mock_paho, _mock_utils):
        """
        Should connect to the broker successfully.
        """
        client = MQTTClient("123", "987", False, False)

        client.connect()
        mock_paho.Client().connect_async.assert_called_once_with(
            host=MOCK_CONFIG['mqtt']['host'],
            port=MOCK_CONFIG['mqtt']['port'],
            keepalive=MOCK_CONFIG['mqtt']['con_timeout'])

        mock_paho.Client().loop_start.assert_called_once()

    def test_connect_error(self, _mock_paho, mock_utils):
        """
        Should not connect to the broker successfully - connect_async error.
        """
        client = MQTTClient("123", "987", False, False)
        client.mqttc.connect_async.side_effect = Exception

        client.connect()

        self.assertRaises(Exception, client.mqttc.connect_async)
        client.mqttc.loop_start.assert_not_called()
        mock_utils.fire_locust_failure.assert_called_once()


@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientDisconnect(unittest.TestCase):
    """
    MQTTClient disconnect() unit tests.
    """
    def test_disconnect(self, _mock_paho):
        """
        Should disconnect correctly.
        """
        client = MQTTClient("123", "987", False, False)

        client.disconnect()

        client.mqttc.disconnect.assert_called_once()


@patch('src.mqtt_locust.mqtt_client.json')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientPublish(unittest.TestCase):
    """
    MQTTClient publish() unit tests.
    """
    def test_publish_success(self, _mock_utils, mock_paho, _mock_json):
        """
        Should publish a message successfully
        """
        mock_paho.Client().publish.return_value = (0, MagicMock())
        client = MQTTClient("123", "987", False, False)

        client.publish()

        mock_paho.Client().publish.assert_called_once()
        keys_len = len(client.pubmmap.keys())
        self.assertGreater(keys_len, 0)

    def test_publish_error(self, mock_utils, mock_paho, _mock_json):
        """
        Should not publish a message successfully
        """
        mock_paho.Client().publish.return_value = (10, MagicMock())
        client = MQTTClient("123", "987", False, False)

        client.publish()
        mock_utils.error_message.assert_called_once_with(10)
        mock_utils.fire_locust_failure.assert_called_once()

        keys_len = len(client.pubmmap.keys())
        self.assertEqual(keys_len, 0)


@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientSubscribe(unittest.TestCase):
    """
    MQTTClient subscribe() unit tests.
    """
    def test_subcribe_success(self, _mock_utils, mock_paho):
        """
        Should subscribe to the topic successfully.
        """
        mock_paho.Client().subscribe.return_value = (0, MagicMock())
        client = MQTTClient("123", "987", False, False)

        client.subscribe()
        mock_paho.Client().subscribe.assert_called_once()
        keys_len = len(client.submmap.keys())
        self.assertGreater(keys_len, 0)

    def test_subscribe_error(self, mock_utils, mock_paho):
        """
        Should not subscribe a message successfully
        """
        mock_paho.Client().subscribe.return_value = (10, MagicMock())
        client = MQTTClient("123", "987", False, False)

        client.subscribe()
        mock_utils.error_message.assert_called_once_with(10)
        mock_utils.fire_locust_failure.assert_called_once()

        keys_len = len(client.pubmmap.keys())
        self.assertEqual(keys_len, 0)


@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientLocustOnSubscribing(unittest.TestCase):
    """
    MQTTClient locust_on_subscribing() unit tests.
    """
    def test_locust_on_subcribing(self, mock_utils, _mock_paho):
        """
        Should fire locust success
        """
        mid = MagicMock()
        client = MQTTClient("123", "987", False, False)
        client.submmap[mid] = {'name': 'name', 'start_time': 'time'}
        client.locust_on_subscribe(client.mqttc, {}, mid, 0)
        mock_utils.fire_locust_success.assert_called_once()

    def test_locust_on_subcribing_failure(self, mock_utils, _mock_paho):
        """
        Should fire locust failure
        """
        client = MQTTClient("123", "987", False, False)

        client.locust_on_subscribe(client.mqttc, {}, 0, 0)
        mock_utils.fire_locust_failure.assert_called_once()


@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientLocustOnPublish(unittest.TestCase):
    """
    MQTTClient locust_on_publish() unit tests.
    """
    def test_locust_on_publish(self, mock_utils, _mock_paho):
        """
        Should fire locust success on publish callback
        """
        mid = MagicMock()
        client = MQTTClient("123", "987", False, False)
        client.pubmmap[mid] = {'name': 'name',
                               'start_time': 'time', 'payload': 'payload'}
        client.locust_on_publish(client.mqttc, {}, mid)
        mock_utils.fire_locust_success.assert_called_once()

    def test_locust_on_publish_failure(self, mock_utils, _mock_paho):
        """
        Should fire locust failure on publish
        """
        client = MQTTClient("123", "987", False, False)
        client.locust_on_publish(client.mqttc, {}, 0)
        mock_utils.fire_locust_failure.assert_called_once()


@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientLocustOnConnect(unittest.TestCase):
    """
    MQTTClient locust_on_connect() unit tests.
    """
    def test_locust_on_connect(self, mock_utils, mock_paho):
        """
        Should fire locust success on connection callback
        """
        mock_paho.MQTT_ERR_SUCCESS = 1
        client = MQTTClient("123", "987", False, False)
        client.subscribe = MagicMock()
        client.locust_on_connect(client.mqttc, {}, {}, 1)
        client.subscribe.assert_called_once()
        mock_utils.fire_locust_success.assert_called_once()

    def test_locust_on_connect_failure(self, mock_utils, mock_paho):
        """
        Should fire locust failure on connection callback
        """
        mock_paho.MQTT_ERR_SUCCESS = 1
        client = MQTTClient("123", "987", False, False)
        client.locust_on_connect(client.mqttc, {}, {}, 101010)
        mock_utils.conack_error_message.assert_called_once()
        mock_utils.fire_locust_failure.assert_called_once()


@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientLocustOnDisconnect(unittest.TestCase):
    """
    MQTTClient locust_on_disconnect() unit tests.
    """
    def test_locust_on_disconnect(self, mock_utils, mock_paho):
        """
        Should fire locust failure on disconnect callback
        """
        mock_paho.MQTT_ERR_SUCCESS = 1
        client = MQTTClient("123", "987", False, False)
        client.locust_on_disconnect(client.mqttc, {}, 1010)

        self.assertFalse(client.is_connected)
        mock_utils.fire_locust_failure.assert_called_once()
        mock_paho.Client().reconnect.assert_called_once()

    def test_locust_on_disconnect_fail(self, _mock_utils, mock_paho):
        """
        Should fire locust failure on disconnect callback
        """
        mock_paho.MQTT_ERR_SUCCESS = 1
        client = MQTTClient("123", "987", False, False)
        client.locust_on_disconnect(client.mqttc, {}, 1)

        self.assertFalse(client.is_connected)
        mock_paho.Client().reconnect.assert_called_once()


@patch('src.mqtt_locust.mqtt_client.json')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientLocustOnMessage(unittest.TestCase):
    """
    MQTTClient locust_on_message() unit tests.
    """
    def test_locust_on_message(self, mock_utils, _mock_paho, mock_json):
        """
        Should fire locust sucess on message callback
        """
        message: mqtt.MQTTMessage = mqtt.MQTTMessage()
        message.payload = str.encode(str({"timestamp": 0}))
        mock_json.loads.return_value = {"timestamp": 0}

        client = MQTTClient("123", "987", False, False)

        client.locust_on_message(client.mqttc, {}, message)

        mock_utils.fire_locust_success.assert_called_once()

    def test_locust_on_message_exception(self, _mock_utils, _mock_paho, _mock_json):
        """
        Should raise an exception on message callback
        """
        client = MQTTClient("123", "987", False, False)

        with self.assertRaises(Exception):
            client.locust_on_message(client.mqttc, {}, {})

        client.locust_on_message(client.mqttc, {}, None)


@patch('src.mqtt_locust.mqtt_client.CertUtils')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientShouldRenewNow(unittest.TestCase):
    """
    MQTTClient should_renew_now() unit tests.
    """
    def test_should_renew_now(self, mock_utils, _mock_paho, _mock_cert_utils):
        """
        Should renew the certificate now.
        """
        client = MQTTClient("123", "987", False, True)

        mock_utils.seconds_to_milliseconds.return_value = 200

        self.assertTrue(client.should_renew_now())

    def test_should_not_renew_now(self, _mock_utils, _mock_paho, _mock_cert_utils):
        """
        Should not renew the certificate now.
        """
        client = MQTTClient("123", "987", False, False)

        self.assertFalse(client.should_renew_now())


@patch('src.mqtt_locust.mqtt_client.CertUtils')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientShouldRevokeNow(unittest.TestCase):
    """
    MQTTClient should_revoke_now() unit tests.
    """
    def test_should_revoke_now(self, mock_utils, _mock_paho, _mock_cert_utils):
        """
        Should revoke the certificate now.
        """
        client = MQTTClient("123", "987", True, False)
        mock_utils.seconds_to_milliseconds.return_value = 200

        self.assertTrue(client.should_revoke_now())

    def test_should_not_revoke_now(self, mock_utils, _mock_paho, _mock_cert_utils):
        """
        Should not revoke the certificate now.
        """
        client = MQTTClient("123", "987", False, False)
        mock_utils.seconds_to_milliseconds.return_value = 50

        self.assertFalse(client.should_revoke_now())


@patch('src.mqtt_locust.mqtt_client.CertUtils')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientRenewCert(unittest.TestCase):
    """
    MQTTClient renew_cert() unit tests.
    """
    def test_renew_cert(self, _mock_paho, _mock_cert_utils):
        "Should renew cert"

        client = MQTTClient("123", "987", False, True)
        client.should_renew_now = MagicMock()
        client.should_renew_now.return_value = True
        client.renew_cert_and_emit_event = MagicMock()
        client.renew_cert_and_emit_event.return_value = True

        client.renew_cert()

        self.assertFalse(client.should_renew)
        client.should_renew_now.assert_called()
        client.renew_cert_and_emit_event.assert_called()

    def test_not_renew_cert(self, _mock_paho, _mock_cert_utils):
        "Should not renew cert"

        client = MQTTClient("123", "987", False, False)
        client.should_renew_now = MagicMock()
        client.should_renew_now.return_value = False
        client.renew_cert_and_emit_event = MagicMock()

        client.renew_cert()

        self.assertFalse(client.should_renew)
        client.should_renew_now.assert_called()
        self.assertTrue(client.renew_cert_and_emit_event.call_count == 0)


@patch('src.mqtt_locust.mqtt_client.CertUtils')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientRevokeCert(unittest.TestCase):
    """
    MQTTClient revoke_cert() unit tests.
    """
    def test_revoke_cert(self, _mock_paho, _mock_cert_utils):
        "Should revoke cert"

        client = MQTTClient("123", "987", True, False)
        client.should_revoke_now = MagicMock()
        client.should_revoke_now.return_value = True
        client.revoke_cert_and_emit_event = MagicMock()
        client.revoke_cert_and_emit_event.return_value = True

        client.revoke_cert()

        self.assertFalse(client.should_revoke)
        client.should_revoke_now.assert_called()
        client.revoke_cert_and_emit_event.assert_called()

    def test_not_revoke_cert(self, _mock_paho, _mock_cert_utils):
        "Should not revoke cert"

        client = MQTTClient("123", "987", False, False)
        client.should_revoke_now = MagicMock()
        client.should_revoke_now.return_value = False
        client.revoke_cert_and_emit_event = MagicMock()

        client.revoke_cert()

        self.assertFalse(client.should_revoke)
        client.should_revoke_now.assert_called()
        self.assertTrue(client.revoke_cert_and_emit_event.call_count == 0)


@patch('src.mqtt_locust.mqtt_client.CertUtils')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientRenewCertAndEmitEvent(unittest.TestCase):
    """
    MQTTClient renew_cert_and_emit_event() unit tests.
    """
    def test_renew_cert_and_emit_event(self, mock_utils, _mock_paho, mock_cert_utils):
        "Should renew cert"
        client = MQTTClient("123", "987", False, True)

        client.revoke_cert_and_emit_event = MagicMock()
        client.revoke_cert_and_emit_event.return_value = True
        mock_cert_utils.create_cert_files = MagicMock()

        has_renewed = client.renew_cert_and_emit_event()

        client.revoke_cert_and_emit_event.assert_called_once()
        client.new_cert.renew_cert.assert_called_once()
        mock_cert_utils.create_cert_files.assert_called_once()
        mock_utils.fire_locust_success.assert_called_once()
        self.assertTrue(has_renewed)

    def test_renew_cert_and_emit_event_exception(self, mock_utils, _mock_paho, mock_cert_utils):
        "Should raise an exception when renewing cert"
        client = MQTTClient("123", "987", False, True)

        client.revoke_cert_and_emit_event = MagicMock()
        client.revoke_cert_and_emit_event.return_value = True
        mock_cert_utils.create_cert_files = MagicMock()
        client.new_cert.renew_cert.side_effect = Exception

        has_renewed = client.renew_cert_and_emit_event()

        client.revoke_cert_and_emit_event.assert_called_once()
        self.assertRaises(Exception, client.new_cert.renew_cert)
        self.assertTrue(mock_cert_utils.create_cert_files.call_count == 0)
        mock_utils.fire_locust_failure.assert_called()
        self.assertFalse(has_renewed)

    def test_not_renew_cert_and_emit_event(self, mock_utils, _mock_paho, mock_cert_utils):
        "Should not renew cert"
        client = MQTTClient("123", "987", False, True)

        client.revoke_cert_and_emit_event = MagicMock()
        client.revoke_cert_and_emit_event.return_value = False
        mock_cert_utils.create_cert_files = MagicMock()

        has_renewed = client.renew_cert_and_emit_event()

        client.revoke_cert_and_emit_event.assert_called_once()
        self.assertTrue(client.new_cert.renew_cert.call_count == 0)
        self.assertTrue(mock_cert_utils.create_cert_files.call_count == 0)
        mock_utils.fire_locust_failure.assert_called()
        self.assertFalse(has_renewed)


@patch('src.mqtt_locust.mqtt_client.CertUtils')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientRevokeCertAndEmitEvent(unittest.TestCase):
    """
    MQTTClient revoke_cert_and_emit_event() unit tests.
    """
    def test_revoke_cert_and_emit_event(self, mock_utils, _mock_paho, mock_cert_utils):
        """
        Should revoke cert
        """
        client = MQTTClient("123", "987", True, False)

        mock_cert_utils.has_been_revoked.return_value = False

        has_revoked = client.revoke_cert_and_emit_event()

        self.assertTrue(has_revoked)
        mock_cert_utils.revoke_cert.assert_called_once()
        mock_utils.fire_locust_success.assert_called()


    def test_not_revoke_cert_and_emit_event(self, mock_utils, _mock_paho, mock_cert_utils):
        """
        Should not revoke cert - Exception thrown by the revoking function
        """
        client = MQTTClient("123", "987", True, False)

        mock_cert_utils.has_been_revoked.return_value = False
        mock_cert_utils.revoke_cert.side_effect = Exception

        has_revoked = client.revoke_cert_and_emit_event()

        self.assertFalse(has_revoked)
        mock_cert_utils.revoke_cert.assert_called_once()
        mock_utils.fire_locust_failure.assert_called()


@patch('src.mqtt_locust.mqtt_client.json')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch('src.mqtt_locust.mqtt_client.CertUtils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class MQTTClientReconnect(unittest.TestCase):
    """
    MQTTClient reconnect() unit tests.
    """
    def test_reconnect(self, _mock_json, _mock_paho, _mock_utils, _mock_cert_utils):
        """
        Should reconnect successfully.
        """
        client = MQTTClient("123", "987", False, False)

        client.reconnect()

        client.mqttc.reconnect.assert_called_once()

    def test_reconnect_after_revoke(self, _mock_json, _mock_paho, _mock_utils, _mock_cert_utils):
        """
        Should reconnect successfully after being revoked.
        """
        client = MQTTClient("123", "987", True, False)
        client.is_revoked = True

        mock_configure_mqtt = patch.object(client, "configure_mqtt").start()
        mock_connect = patch.object(client, "connect").start()
        mock_mqttc = patch.object(client, "mqttc").start()

        client.reconnect()

        mock_mqttc.loop_stop.assert_called_once()
        mock_connect.assert_called_once()
        mock_configure_mqtt.assert_called_once()

        mock_connect.stop()
        mock_configure_mqtt.stop()
        mock_mqttc.stop()

    def test_reconnect_after_renew(self, _mock_json, _mock_paho, _mock_utils, _mock_cert_utils):
        """
        Should reconnect successfully after being renewed.
        """
        client = MQTTClient("123", "987", False, True)
        client.is_renewed = True

        mock_configure_mqtt = patch.object(client, "configure_mqtt").start()
        mock_connect = patch.object(client, "connect").start()
        mock_mqttc = patch.object(client, "mqttc").start()

        client.reconnect()

        mock_mqttc.loop_stop.assert_called_once()
        mock_connect.assert_called_once()
        mock_configure_mqtt.assert_called_once()

        mock_connect.stop()
        mock_configure_mqtt.stop()
        mock_mqttc.stop()

    def test_not_reconnect(self, _mock_json, _mock_paho, _mock_utils, _mock_cert_utils):
        """
        Should not reconnect because disconnect_forever is True
        """
        client = MQTTClient("123", "987", False, False)
        client.disconnect_forever = True

        mock_configure_mqtt = patch.object(client, "configure_mqtt").start()
        mock_connect = patch.object(client, "connect").start()
        mock_mqttc = patch.object(client, "mqttc").start()

        client.reconnect()

        mock_mqttc.loop_stop.assert_not_called()
        mock_connect.assert_not_called()
        mock_configure_mqtt.assert_not_called()

        mock_mqttc.reconnect.assert_not_called()

        mock_connect.stop()
        mock_configure_mqtt.stop()
        mock_mqttc.stop()


if __name__ == "__main__":
    unittest.main()
