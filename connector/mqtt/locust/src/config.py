"""Environment configuration related module. """

import os

from src.utils import Utils

CONFIG = {
    'app': {
        'tenant':       os.environ.get("TENANT", "admin"),
    },

    'security': {
        'devices_to_renew':           int(os.environ.get("DEVICES_TO_RENEW", 1000)),
        'devices_to_revoke':          int(os.environ.get("DEVICES_TO_REVOKE", 1000)),
        'dns_cert':                   [],
        'ejbca_ca_name':              "IOTmidCA",
        'ejbca_url':                  os.environ.get("EJBCA_URL", "http://localhost:5583"),
        'renew_devices':              Utils.str_to_bool(os.environ.get("RENEW_DEVICES", "False")),
        'revoke_devices':             Utils.str_to_bool(os.environ.get("REVOKE_DEVICES", "False")),
        'cert_dir':                   os.environ.get("CERT_DIR", "cert/"),
        'renew_cert_dir':             os.environ.get("RENEW_CERT_DIR", "renew/"),
        'revoke_cert_dir':            os.environ.get("REVOKE_CERT_DIR", "revoke/"),
        'ca_cert_file':               os.environ.get("CA_CERT_FILE", "ca.crt"),
        'time_to_renew':              int(os.environ.get("TIME_TO_RENEW", 1000)),
        'time_to_revoke':             int(os.environ.get("TIME_TO_REVOKE", 1000)),
        'max_time_reconn':            int(os.environ.get("MAX_TIME_RECONN", 600)),
        'min_time_reconn':            int(os.environ.get("MIN_TIME_RECONN", 1)),
        'probability_to_renew':       float(os.environ.get("PROBABILITY_TO_RENEW", 10.0)),
        'probability_to_revoke':      float(os.environ.get("PROBABILITY_TO_REVOKE", 10.0))
    },

    'locust': {
        'task_min_time':        int(os.environ.get("TASK_MIN_TIME", 29500)),
        'task_max_time':        int(os.environ.get("TASK_MAX_TIME", 30000)),
        'log_dir':              os.environ.get("LOCUST_LOG_DIR", "/log"),
        'redis': {
            'certificates_db':      int(os.environ.get("REDIS_CERTIFICATES_DB", 0)),
            'mapped_db':            int(os.environ.get("REDIS_MAPPED_DB", 1)),
            'host':                 os.environ.get("REDIS_HOST", "127.0.0.1"),
            'port':                 int(os.environ.get("REDIS_PORT", 6379)),
        }
    },

    'mqtt': {
        'host':         os.environ.get("DOJOT_MQTT_HOST", "127.0.0.1"),
        'port':         int(os.environ.get("DOJOT_MQTT_PORT", 1883)),
        'con_timeout':  int(os.environ.get("DOJOT_MQTT_TIMEOUT", 120)),
        'qos':          int(os.environ.get("DOJOT_MQTT_QOS", 1)),
        'pub_timeout':  int(os.environ.get("MQTT_PUBLISH_TIMEOUT", 40000)),
        'sub_timeout':  int(os.environ.get("MQTT_SUBSCRIBE_TIMEOUT", 40000)),
    },
}
