"""
    Handles Paho MQTT-Client operations like publish/subscription, connection,
    loop function.
"""
import json
import logging
import time

import paho.mqtt.client as mqtt
from src.config import CONFIG
from src.ejbca.cert_utils import CertUtils
from src.utils import Utils

REQUEST_TYPE = 'mqtt'
MESSAGE_TYPE_CONNECT = 'connect'
MESSAGE_TYPE_DISCONNECT = 'disconnect'
MESSAGE_TYPE_PUB = 'publish'
MESSAGE_TYPE_SUB = 'subscribe'
MESSAGE_TYPE_RECV_MESSAGE = 'recv_message'
MESSAGE_TYPE_RENEW = 'renew'
MESSAGE_TYPE_REVOKE = 'revoke'

logger = Utils.create_logger("mqtt_client")


class LocustError(Exception):
    """
    Locust error exception.
    """


class ConnectError(Exception):
    """
    Connection error exception.
    """


class DisconnectError(Exception):
    """
    Disconnection error exception.
    """


class CertRevogationError(Exception):
    """
    Certificate revogation error exception.
    """


class CertRenovationError(Exception):
    """
    Certificate renovation error exception.
    """


class MQTTClient:
    """
    MQTT client to load test Dojot MQTT IoTAgent.
    """

    def __init__(self,
                 device_id: str,
                 run_id: str,
                 should_revoke: bool,
                 should_renew: bool):
        """
        MQTT client constructor. To get this to work, you should call setup() after instantiating
        the class.

        Args:
            device_id: device identifier
            run_id: client run identifier
            should_revoke: whether this client should have its certificate revoked
            should_renew: whether this client should have its certificate renewed
        """
        Utils.validate_tenant(CONFIG["app"]["tenant"])
        Utils.validate_device_id(device_id)

        if len(run_id) < 1:
            raise ValueError("the run ID must have at least one character")

        if should_renew and should_revoke:
            raise ValueError("only one of should_renew and should_revoke can be True")

        self.device_id = device_id
        self.run_id = run_id
        self.should_revoke = should_revoke
        self.should_renew = should_renew
        self.is_renewed = False
        self.is_revoked = False

        self.is_connected = False
        # If this variable is set, the on_disconnect callback will not try to reconnect
        # the device again
        self.disconnect_forever = False
        self.mqttc = None

        self.tenant = CONFIG["app"]["tenant"]
        self.username = '{0}:{1}'.format(self.tenant, self.device_id)
        self.topic = "{0}/attrs".format(self.username)
        self.sub_topic = "{0}/config".format(self.username)

        self.device_cert_dir = CONFIG["security"]["cert_dir"]
        self.new_cert = None

        self.pubmmap = {}
        self.submmap = {}

        # Used to count the time between connection and revocation/renovation
        self.start_time = 0

        self.create_certificate()
        self.configure_mqtt()

    def create_certificate(self) -> None:
        """
        Create the certificate for renovation/revocation.
        """
        if self.should_revoke:
            self.device_cert_dir = self.device_cert_dir + CONFIG["security"]["revoke_cert_dir"]
            self.new_cert = CertUtils.new_cert(self.tenant, self.device_id)
            CertUtils.create_cert_files(self.new_cert, self.device_cert_dir)

        elif self.should_renew:
            self.device_cert_dir = self.device_cert_dir + CONFIG["security"]["renew_cert_dir"]
            self.new_cert = CertUtils.new_cert(self.tenant, self.device_id)
            CertUtils.create_cert_files(self.new_cert, self.device_cert_dir)

    def configure_mqtt(self) -> None:
        """
        Configures the MQTT connection.
        """
        # Certification files
        cert_dir = CONFIG["security"]["cert_dir"]
        ca_cert_file = cert_dir + CONFIG["security"]["ca_cert_file"]
        cert_file = self.device_cert_dir + CertUtils.get_certificate_file(self.device_id)
        key_file = self.device_cert_dir + CertUtils.get_private_key_file(self.device_id)

        # Configuring MQTT client
        self.mqttc = mqtt.Client(client_id=self.device_id)

        # Sets exponential reconnect delay
        self.mqttc.reconnect_delay_set(
            min_delay=CONFIG["security"]["min_time_reconn"],
            max_delay=CONFIG["security"]["max_time_reconn"]
        )

        # Setting up TLS
        self.mqttc.tls_set(ca_cert_file, cert_file, key_file)
        self.mqttc.tls_insecure_set(True)

        # Registering MQTT client callbacks
        self.mqttc.on_connect = self.locust_on_connect
        self.mqttc.on_disconnect = self.locust_on_disconnect
        self.mqttc.on_publish = self.locust_on_publish
        self.mqttc.on_subscribe = self.locust_on_subscribe
        self.mqttc.on_message = self.locust_on_message

    def connect(self) -> None:
        """
        Connects to MQTT host.
        """

        try:
            self.mqttc.connect_async(host=CONFIG['mqtt']['host'], port=CONFIG['mqtt']['port'],
                                     keepalive=CONFIG['mqtt']['con_timeout'])
            self.mqttc.loop_start()
        except Exception as exception:
            logging.error("Error while connecting to the broker: %s", str(exception))
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name='connect',
                response_time=0,
                exception=ConnectError("disconnected")
            )

    def disconnect(self) -> None:
        """
        Disconnects from the MQTT broker.
        """
        self.disconnect_forever = True
        self.mqttc.disconnect()

    def publish(self, payload: dict = None, send_timestamp: bool = True) -> None:
        """
        Handles the publishing of messages to MQTT host.
        """
        start_time = Utils.seconds_to_milliseconds(time.time())
        try:
            if payload is None:
                payload = dict()

            if not isinstance(payload, dict):
                raise ValueError('Payload must be a dict')

            if send_timestamp:
                payload["timestamp"] = start_time

            err, mid = self.mqttc.publish(
                topic=self.topic,
                payload=json.dumps(payload),
                qos=CONFIG['mqtt']['qos']
            )

            if err:
                raise ValueError(err)

            self.pubmmap[mid] = {
                'name': MESSAGE_TYPE_PUB,
                'payload': payload,
                'start_time': start_time
            }

        except Exception as exception:
            error = Utils.error_message(int(str(exception)))

            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_PUB,
                response_time=Utils.seconds_to_milliseconds(time.time()) - start_time,
                exception=error
            )

    def subscribe(self) -> None:
        """
        Handles the subscription in MQTT topics.
        """

        start_time = Utils.seconds_to_milliseconds(time.time())

        try:
            err, mid = self.mqttc.subscribe((self.sub_topic, CONFIG['mqtt']['qos']))

            if err:
                raise ValueError(err)

            self.submmap[mid] = {
                'name': MESSAGE_TYPE_SUB,
                'start_time': start_time
            }

        except Exception as exception:
            error = Utils.error_message(int(str(exception)))
            logging.error("Error while subscribing: %s", error)

            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_SUB,
                response_time=Utils.seconds_to_milliseconds(time.time()) - start_time,
                exception=error
            )

    def reconnect(self) -> None:
        """
        Try to reconnect to the broker.
        """
        if not self.disconnect_forever:
            # When reconnect() is called, it calls the Paho connect() with the same parameters as
            # before, so if we change the certificate, we need to pass new values to it
            if self.is_revoked or self.is_renewed:
                self.mqttc.loop_stop()
                self.mqttc = None
                self.configure_mqtt()
                self.connect()
                self.is_revoked = False
                self.is_renewed = False
            else:
                self.mqttc.reconnect()

    ###############
    ## Callbacks ##
    ###############

    def locust_on_subscribe(
            self,
            _client: mqtt.Client,
            _userdata,
            mid,
            _granted_qos) -> None:
        """
        Subscription callback function.
        """
        end_time = Utils.seconds_to_milliseconds(time.time())
        message = self.submmap.pop(mid, None)

        if message is None:
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_SUB,
                response_time=0,
                exception=ValueError("Subscription not found"),
            )

        else:
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=message['name'],
                response_time=end_time - message['start_time'],
                response_length=0
            )

    def locust_on_publish(self, _client: mqtt.Client, _userdata, mid) -> None:
        """
        Publishing callback function.
        """
        end_time = Utils.seconds_to_milliseconds(time.time())
        message = self.pubmmap.pop(mid, None)

        if message is None:
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_PUB,
                response_time=0,
                exception=ValueError("Published message could not be found"),
            )

        else:
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=message['name'],
                response_time=end_time - message['start_time'],
                response_length=len(message['payload']),
            )

    def locust_on_connect(
            self,
            _client: mqtt.Client,
            _flags_dict,
            _userdata,
            result_code: int) -> None:
        """
        Connection callback function.
        """
        if result_code == mqtt.CONNACK_ACCEPTED:
            self.subscribe()
            self.is_connected = True
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_CONNECT,
                response_time=0,
                response_length=0
            )
            self.start_time = Utils.seconds_to_milliseconds(time.time())
        else:
            error = Utils.conack_error_message(result_code)
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_CONNECT,
                response_time=0,
                exception=ConnectError(error)
            )

    def locust_on_disconnect(self, _client: mqtt.Client, _userdata, result_code: int) -> None:
        """
        Disconnection callback function.
        """
        if result_code != mqtt.MQTT_ERR_SUCCESS:
            self.is_connected = False
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_DISCONNECT,
                response_time=0,
                exception=DisconnectError(Utils.error_message(result_code))
            )

        self.reconnect()

    def locust_on_message(self, _client: mqtt.Client, _userdata, message: mqtt.MQTTMessage):
        """
        Message reception callback function.
        """
        if message is not None:
            publish_time = 0.0
            try:
                publish_time = float(json.loads(message.payload.decode())["timestamp"])
            except Exception as exception:
                logging.error("Error while parsing the message payload: %s", str(exception))
                raise Exception(str(exception))
            else:
                Utils.fire_locust_success(
                    request_type=REQUEST_TYPE,
                    name=MESSAGE_TYPE_RECV_MESSAGE,
                    response_time=Utils.seconds_to_milliseconds(time.time()) - publish_time,
                    response_length=len(message.payload)
                )

    #################
    ## Certificate ##
    #################
    def renew_cert(self) -> None:
        """
        Renew a certificate and emit an event whether it succeeded or not.
        """
        if self.should_renew_now():
            self.should_renew = not self.renew_cert_and_emit_event()

    def renew_cert_and_emit_event(self) -> bool:
        """
        Renew a certificate and emit an event whether it succeeded or not.

        Return: True if succeeded, False otherwise.
        """
        try:
            # First we need to revoke the certificate
            if not self.revoke_cert_and_emit_event():
                raise CertRevogationError()

            self.new_cert.renew_cert()
            CertUtils.create_cert_files(self.new_cert, self.device_cert_dir)

        except Exception as exception:
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_RENEW,
                response_time=0,
                exception=CertRenovationError("failed to renew")
            )
            logging.error("An error occurred while trying to renew the certificate")
            logging.error(str(exception))
            return False

        else:
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_RENEW,
                response_time=0,
                response_length=0
            )
            self.is_renewed = True
            return True

    def revoke_cert(self) -> None:
        """
        Verifies whether the certificate should be revoked or not and then
        effectively revoke it.
        """
        if self.should_revoke_now():
            self.should_revoke = not self.revoke_cert_and_emit_event()

    def revoke_cert_and_emit_event(self) -> bool:
        """
        Revoke a certificate and emit an event whether it succeeded or not.

        Return: True if succeeded, False otherwise.
        """
        try:
            if CertUtils.has_been_revoked(self.new_cert):
                logging.debug("Already revoked, skipping step...")
            else:
                CertUtils.revoke_cert(self.new_cert)
                Utils.fire_locust_success(
                    request_type=REQUEST_TYPE,
                    name=MESSAGE_TYPE_REVOKE,
                    response_time=0,
                    response_length=0
                )
                self.is_revoked = True

            return True

        except Exception as exception:
            logging.error("An error occurred while trying to revoke the certificate")
            logging.error(str(exception))

        Utils.fire_locust_failure(
            request_type=REQUEST_TYPE,
            name=MESSAGE_TYPE_REVOKE,
            response_time=0,
            exception=CertRevogationError("certificate not revoked")
        )
        return False

    def should_renew_now(self) -> bool:
        """
        Verifies if the conditions to renew the certificate were satisfied.
        """
        end_time = Utils.seconds_to_milliseconds(time.time())
        return self.should_renew and \
               end_time - self.start_time >= CONFIG["security"]["time_to_renew"]

    def should_revoke_now(self) -> bool:
        """
        Verifies if the conditions to revoke the certificate were satisfied.
        """
        end_time = Utils.seconds_to_milliseconds(time.time())
        return self.should_revoke and \
               end_time - self.start_time >= CONFIG["security"]["time_to_revoke"]
