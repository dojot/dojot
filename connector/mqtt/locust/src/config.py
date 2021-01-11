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
        'redis': {
            'certificates_db':      int(os.environ.get("REDIS_CERTIFICATES_DB", 0)),
            'mapped_db':            int(os.environ.get("REDIS_MAPPED_DB", 1)),
            'host':                 os.environ.get("REDIS_HOST", "redis"),
            'port':                 int(os.environ.get("REDIS_PORT", 6379)),
            'jwt_expire_time':      int(os.environ.get("REDIS_STORED_JWT_EXPIRE_TIME", 1800)),
        }
    },

    'mqtt': {
        'host':         os.environ.get("DOJOT_MQTT_HOST", "127.0.0.1"),
        'port':         int(os.environ.get("DOJOT_MQTT_PORT", 1883)),
        'con_timeout':  int(os.environ.get("DOJOT_MQTT_TIMEOUT", 120)),
        'qos':          int(os.environ.get("DOJOT_MQTT_QOS", 1)),
    },

    'dojot': {
        'url':      os.environ.get("DOJOT_URL", "http://127.0.0.1:8000"),
        'user':     os.environ.get("DOJOT_USER", "admin"),
        'passwd':   os.environ.get("DOJOT_PASSWD", "admin"),
        'env':      (str(os.environ.get("DOJOT_ENV", "n")).lower() == "y"),
        'api': {
            'retries':      int(os.environ.get("DOJOT_API_RETRIES", 3)),
            'time':         float(os.environ.get("DOJOT_API_RETRY_TIME", 5000.0))  / 1000.0,
            'page_size':    int(os.environ.get("DOJOT_DEVICES_PAGE_SIZE", 20))
        }
    },
}
