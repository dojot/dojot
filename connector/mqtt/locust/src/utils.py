"""
Utils functions.
"""
import logging
import sys
import os
import random

from decimal import getcontext
import paho.mqtt.client as mqtt
from locust import events

MQTT_CONACK_ERRORS = {
    mqtt.CONNACK_ACCEPTED: "CONNACK_ACCEPTED",
    mqtt.CONNACK_REFUSED_PROTOCOL_VERSION: "CONNACK_REFUSED_PROTOCOL_VERSION",
    mqtt.CONNACK_REFUSED_IDENTIFIER_REJECTED: "CONNACK_REFUSED_IDENTIFIER_REJECTED",
    mqtt.CONNACK_REFUSED_SERVER_UNAVAILABLE: "CONNACK_REFUSED_SERVER_UNAVAILABLE",
    mqtt.CONNACK_REFUSED_BAD_USERNAME_PASSWORD: "CONNACK_REFUSED_BAD_USERNAME_PASSWORD",
    mqtt.CONNACK_REFUSED_NOT_AUTHORIZED: "CONNACK_REFUSED_NOT_AUTHORIZED",
}

MQTT_ERRORS = {
    mqtt.MQTT_ERR_AGAIN: "MQTT_ERR_AGAIN",
    mqtt.MQTT_ERR_SUCCESS: "MQTT_ERR_SUCCESS",
    mqtt.MQTT_ERR_NOMEM: "MQTT_ERR_NOMEM",
    mqtt.MQTT_ERR_PROTOCOL: "MQTT_ERR_PROTOCOL",
    mqtt.MQTT_ERR_INVAL: "MQTT_ERR_INVAL",
    mqtt.MQTT_ERR_NO_CONN: "MQTT_ERR_NO_CONN",
    mqtt.MQTT_ERR_CONN_REFUSED: "MQTT_ERR_CONN_REFUSED",
    mqtt.MQTT_ERR_NOT_FOUND: "MQTT_ERR_NOT_FOUND",
    mqtt.MQTT_ERR_CONN_LOST: "MQTT_ERR_CONN_LOST",
    mqtt.MQTT_ERR_TLS: "MQTT_ERR_TLS",
    mqtt.MQTT_ERR_PAYLOAD_SIZE: "MQTT_ERR_PAYLOAD_SIZE",
    mqtt.MQTT_ERR_NOT_SUPPORTED: "MQTT_ERR_NOT_SUPPORTED",
    mqtt.MQTT_ERR_AUTH: "MQTT_ERR_AUTH",
    mqtt.MQTT_ERR_ACL_DENIED: "MQTT_ERR_ACL_DENIED",
    mqtt.MQTT_ERR_UNKNOWN: "MQTT_ERR_UNKNOWN",
    mqtt.MQTT_ERR_ERRNO: "MQTT_ERR_ERRNO",
    mqtt.MQTT_ERR_QUEUE_SIZE: "MQTT_ERR_QUEUE_SIZE",
}

LOG_LEVELS = {
    "debug": logging.DEBUG,
    "info": logging.INFO,
    "warning": logging.WARNING,
    "error": logging.ERROR,
    "critical": logging.CRITICAL,
}


class Utils():
    """Project Utils class."""

    # Fix Decimal type precision
    getcontext().prec = 5

    @staticmethod
    def fire_locust_failure(**kwargs):
        """Fires the request_failure event in Locust.

        Args:
            kwargs: Locust event keyword arguments.
        """

        events.request.fire(response_length=0, context={}, **kwargs)

    @staticmethod
    def fire_locust_success(**kwargs):
        """Fires the request_success event in Locust.

        Args:
            kwargs: Locust event keyword arguments.
        """

        events.request.fire(context={}, exception=None, **kwargs)

    @staticmethod
    def str_to_bool(string: str) -> bool:
        """Converts a string to bool.

        Args:
            str (string): string to be converted. Accepted values: 'True', 'False'. Any other
            value will be considered False.

        Returns:
            bool
        """

        return string.lower() == "true"

    @staticmethod
    def conack_error_message(error: int) -> str:
        """Converts the error code from Locust in an understandable string."""

        if MQTT_CONACK_ERRORS.get(error):
            return MQTT_CONACK_ERRORS[error]

        return "{}\n".format(error)

    @staticmethod
    def error_message(error: int) -> str:
        """Converts the error code from Locust in an understandable string."""

        if MQTT_ERRORS.get(error):
            return MQTT_ERRORS[error]

        return "{}\n".format(error)

    @staticmethod
    def is_master() -> bool:
        """
        Checks if the code is being run by the Locust master.
        """
        return "--master" in sys.argv

    @staticmethod
    def is_slave() -> bool:
        """
        Checks if the code is being run by a Locust slave.
        """
        return "--slave" in sys.argv

    @staticmethod
    def log_level(level_name: str) -> int:
        """
        Parses the log level and returns the correct values for the logging package.

        Args:
            level_name: wanted log level.

        Returns the value that the logging package understands, defaults to INFO.
        """
        level_name = level_name.lower()

        return LOG_LEVELS[level_name] if LOG_LEVELS.get(level_name) else LOG_LEVELS["info"]

    @staticmethod
    def validate_device_id(device_id: str) -> None:
        """
        Validates the device ID.

        Raises a ValueError when the ID is invalid.
        """
        if len(device_id) < 1:
            raise ValueError("the device ID must have at least one character")

    @staticmethod
    def validate_tenant(tenant: str) -> None:
        """
        Validates the tenant.

        Raises a ValueError when the tenant is invalid.
        """
        if len(tenant) < 1:
            raise ValueError("the tenant name must have at least one character")

    @staticmethod
    def validate_thing_id(thing_id: str) -> None:
        """
        Validates a thing ID. Its format must be tenant:deviceid.

        Raises a ValueError when the thing ID is invalid.
        """
        split = thing_id.split(":")

        if len(split) != 2:
            raise ValueError("the thing ID must be in the format tenant:deviceid")

        Utils.validate_tenant(split[0])
        Utils.validate_device_id(split[1])

    @staticmethod
    def create_thing_id(tenant: str, device_id: str) -> str:
        """
        Create the thing ID.
        """
        Utils.validate_tenant(tenant)
        Utils.validate_device_id(device_id)

        return f"{tenant}:{device_id}"

    @staticmethod
    def seconds_to_milliseconds(time: float) -> int:
        """
        Converts seconds to milliseconds.

        Params:
            time: time in seconds to be converted.
        """
        return int(time * 1000.0)

    @staticmethod
    def should_execute(probability: float) -> bool:
        """
        Draw a number in [0.0, 1.0) and verify if it is less than 'probability'.

        Params:
            probability: value in [0.0, 1.0)

        Returns True if the drafted number is lower than 'probability', False otherwise.
        """
        return random.random() < probability

    @staticmethod
    def create_logger(name: str = "root") -> logging.Logger:
        """
        Create a Logger instance with the configs passed via config.py.

        Params:
            name: name of the logger
        """
        logger = logging.getLogger(name)
        handler = logging.StreamHandler()
        formatter = logging.Formatter(
            fmt="%(name)s line %(lineno)s | %(asctime)s [%(levelname)s]: %(message)s",
            datefmt="%d-%m-%Y %H:%M:%S"
        )
        handler.setFormatter(formatter)
        logger.addHandler(handler)
        logger.setLevel(Utils.log_level(os.environ.get("LOG_LEVEL", "info")))

        return logger
