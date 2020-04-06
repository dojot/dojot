"""
Redis-related module.
"""
import logging
from uuid import uuid4
import redis

from src.config import CONFIG
from src.utils import Utils
from src.dojot.api import DojotAPI

class RedisClient():
    """
    Redis handler class.
    """

    def __init__(self) -> None:
        """
        Attempts to stablish a connection.
        """
        self.logger = Utils.create_logger("redis_client")

        try:
            self.certificates = redis.Redis(
                host=CONFIG['locust']['redis']['host'],
                port=CONFIG['locust']['redis']['port'],
                db=CONFIG['locust']['redis']['certificates_db'])
            self.mapped = redis.Redis(
                host=CONFIG['locust']['redis']['host'],
                port=CONFIG['locust']['redis']['port'],
                db=CONFIG['locust']['redis']['mapped_db'])

        except Exception as exception:
            self.logger.error(str(exception))
            raise

    def next_device_id(self) -> str:
        """
        Retrieves the next device_id in the list.

        It is important that each client has a unique ID to simulate our scenario,
        since there is no repetition in IDs in any scenario.
        """

        try:
            device_count = self.mapped.incr('device_count')
            device_id = self.mapped.get(device_count)
            return device_id.decode('utf-8')
        except Exception as exception:
            self.logger.error(str(exception))
            raise

    def has_to_revoke(self) -> {bool, str}:
        """
        Checks if another device can be listed as to be revoked.

        Returns:
            Dictionary with:
            - should_revoke: whether this device should be revoked
            - device_id: ID to be used by the device
            Or None if should not revoke.
        """
        try:
            if CONFIG['security']['revoke_devices']:
                # Lua script to check if this device should be revoked.
                # We need to make this in a script to guarantee the atomicity
                # of the operations, specially the if condition checking.
                #
                # When running a Lua script, all its operations are atomic and
                # Redis guarantee that, when the script is running, no other
                # operations are run.
                should_revoke = self.mapped.eval(
                    "local n = tonumber(redis.call('get', 'devices_to_revoke'))\n" +
                    "if n < {0} then\n".format(int(CONFIG['security']['devices_to_revoke'])) +
                    "redis.call('incr', 'devices_to_revoke')\n" +
                    "return true\n" +
                    "end\n" +
                    "return false",
                    0
                )

                if should_revoke == 1:
                    return {
                        "should_revoke": True,
                        "device_id": self.get_device_id()
                    }

        except Exception as exception:
            self.logger.error(str(exception))

        return None

    def has_to_renew(self) -> {bool, str}:
        """
        Checks if another device can be listed as to be renewed.

        Returns:
            Dictionary with:
            - should_renew: whether this device should be renewed
            - device_id: ID to be used by the device
            Or None if should not renew.
        """
        try:
            if CONFIG['security']['renew_devices']:
                # Lua script to check if this device should be renewed.
                # We need to make this in a script to guarantee the atomicity
                # of the operations, specially the if condition checking.
                #
                # When running a Lua script, all its operations are atomic and
                # Redis guarantee that, when the script is running, no other
                # operations are run.
                should_renew = self.mapped.eval(
                    "local n = tonumber(redis.call('get', 'devices_to_renew'))\n" +
                    "if n < {0} then\n".format(int(CONFIG['security']['devices_to_renew'])) +
                    "redis.call('incr', 'devices_to_renew')\n" +
                    "return true\n" +
                    "end\n" +
                    "return false",
                    0
                )

                if should_renew == 1:
                    return {
                        "should_renew": True,
                        "device_id": self.get_device_id()
                    }

        except Exception as exception:
            self.logger.error(str(exception))

        return None

    def get_jwt(self):
        """
        Retrieves the JWT from the dadtabase or create a new one.
        """
        jwt = self.mapped.get('jwt')

        if jwt:
            return jwt.decode('utf-8')

        jwt = DojotAPI.get_jwt()
        self.mapped.setex('jwt', CONFIG['locust']['redis']['jwt_expire_time'], jwt)

        return jwt

    def get_template_id(self):
        """
        Retrieves the test template ID from the database or create a new one.
        """
        template_id = self.mapped.get('template_id').decode('utf-8')

        if template_id == "-1":
            jwt = self.get_jwt()
            template_id = DojotAPI.create_template(jwt)
            self.mapped.set('template_id', template_id)

        return template_id

    def get_device_id(self):
        """
        Get the device ID.
        """
        device_id = None

        if CONFIG['dojot']['env']:
            jwt = self.get_jwt()
            template_id = self.get_template_id()
            device_id = DojotAPI.create_device(
                jwt,
                template_id,
                "CargoContainer_{0}".format(str(uuid4()).replace("-", ""))
            )

        else:
            device_id = str(uuid4()).replace("-", "")

        return device_id
