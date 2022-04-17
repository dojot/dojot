"""
Locust Load Test.
"""
import datetime
import json
import logging
import os
import uuid
from itertools import chain
import requests

import gevent
from locust import User, task, between, events, stats
from locust.runners import MasterRunner, STATE_STOPPING, STATE_STOPPED

from src.utils import Utils
from src.config import CONFIG
from src.mqtt_locust.mqtt_client import MQTTClient
from src.mqtt_locust.redis_client import RedisClient

from flask import Blueprint, jsonify


class MqttLocust(User):
    """Locust client using MQTT."""
    abstract = True

    def __init__(self, environment):
        super().__init__(environment)

        # Connects to Redis database that stores the device_id for each client
        cache = RedisClient()

        revoke = cache.has_to_revoke()
        should_revoke = False
        should_renew = False

        device_id = None
        # We need to differentiate the device IDs to be revogated/renewed from the other ones
        # The revogated/renewed ones will not be stored in Redis; instead, they will be created
        # at runtime
        if revoke:
            should_revoke = revoke["should_revoke"]
            device_id = revoke["device_id"]
        else:
            renew = cache.has_to_renew()

            if renew:
                should_renew = renew["should_renew"]
                device_id = renew["device_id"]
            else:
                device_id = cache.next_device_id()

        # UUID to identify the client run
        run_id = str(uuid.uuid4())

        self.client = MQTTClient(device_id, run_id, should_revoke, should_renew)
        self.client.connect()


class Client(MqttLocust):
    """The client that will run the tasks when hatched."""

    min = CONFIG['locust']['task_min_time']
    max = CONFIG['locust']['task_max_time']
    wait_time = between(min, max)

    @task
    def publish(self):
        """Publishes a message to MQTT broker."""
        if self.environment.runner.state != STATE_STOPPING or self.environment.runner.state != STATE_STOPPED:
            if self.client.is_connected:
                if Utils.should_execute(CONFIG['security']['probability_to_revoke'] / 100.0):
                    self.client.revoke_cert()

                if Utils.should_execute(CONFIG['security']['probability_to_renew'] / 100.0):
                    self.client.renew_cert()

                self.client.publish()

    def on_stop(self):
        """
        Treats the client when Locust test has stopped.
        """
        if len(self.client.pubmmap) != 0:
            gevent.sleep(10)
        self.client.disconnect()


test_start_time = 0


@events.test_start.add_listener
def _(environment, **_kwargs):
    global test_start_time
    # in a distributed run, the master does not typically need any test data
    if isinstance(environment.runner, MasterRunner):
        test_start_time = int(datetime.datetime.now().timestamp())
        logging.info('test started at' + str(test_start_time))


path = os.path.dirname(os.path.abspath(__file__))
extend = Blueprint(
    "extend",
    "extend_web_ui"
)


@events.init.add_listener
def locust_init(environment, **kwargs):
    """
    Add influx page to get the difference between publish messages on locust and what is stored in influxdb
    """
    if environment.web_ui:
        @extend.route("/influx")
        def influx_page():
            report = {"test_start_time": test_start_time}

            publish_requests = 0
            influx_points = 0
            difference = None

            for s in chain(stats.sort_stats(environment.runner.stats.entries), [environment.runner.stats.total]):
                if s.name == 'publish':
                    publish_requests = s.num_requests

            try:
                influx_points = int(get_influx_stat(test_start_time))
                difference = publish_requests - influx_points
            except TimeoutError or IndexError as e:
                logging.error('Error getting influx data')
                influx_points = 'ERROR'

            report['locust_publish_requests'] = publish_requests
            report['influx_points'] = influx_points
            report['difference'] = difference
            return jsonify(report)

        # register our new routes and extended UI with the Locust web UI
        environment.web_ui.app.register_blueprint(extend)


def get_influx_stat(time):
    url = f"http://{CONFIG['influxdb']['host']}:{CONFIG['influxdb']['port']}/api/v2/query"
    parameters = {'org': CONFIG['influxdb']['org']}
    headers = {'Content-Type': 'application/json', 'Authorization': f"Token {CONFIG['influxdb']['token']}"}
    body = {
        "query": f"from(bucket: \"devices\")"
                 f"    |> range(start: {time})"
                 f"    |> group()"
                 f"    |> count(column: \"_value\")",
        "type": "flux"
    }
    try:
        response = requests.post(url, params=parameters, headers=headers, data=json.dumps(body))
        num_publication = response.text.splitlines()[1].split(',')[5]
    except IndexError:
        logging.error("Parse error: cannot get measurement number.")
        raise
    except TimeoutError:
        logging.error("Influx is not reachable.")
        raise
    return num_publication
