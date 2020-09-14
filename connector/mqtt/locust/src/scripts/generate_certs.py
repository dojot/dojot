"""
Certificate creation for Locust.
"""

import argparse
import glob
import multiprocessing
import time
import uuid
import shutil
import sys
import os
from typing import List, Tuple
import redis

from src.ejbca.thing import Thing
from src.config import CONFIG
from src.utils import Utils
from src.dojot.api import DojotAPI


LOGGER = Utils.create_logger("generate_cert")


class GenerateCerts():
    """
    Certificate and Dojot certificate/device manager.
    """
    def __init__(self):
        """
        Creates the parsers and subparsers.
        """
        parser = argparse.ArgumentParser()

        # Creating subparsers
        subparsers = parser.add_subparsers(dest="topic")

        dojot_parser = subparsers.add_parser("dojot")
        cert_parser = subparsers.add_parser("cert")
        redis_parser = subparsers.add_parser("redis")

        # Creating Dojot parsers
        subparsers = dojot_parser.add_subparsers(dest="dojot")
        dojot_create_parser = subparsers.add_parser("create")
        dojot_clear_parser = subparsers.add_parser("clear")

        self.config_dojot_clear_parser(dojot_clear_parser)
        self.config_dojot_create_parser(dojot_create_parser)
        self.config_cert_parser(cert_parser)
        self.config_redis_parser(redis_parser)

        self.parser_args = parser.parse_args()

        self.jwt = None

        self.run()


    ## Parsers' arguments definitions ##
    @staticmethod
    def config_dojot_clear_parser(parser):
        """
        Configures the parser arguments for Dojot clear parser.
        """
        clear_me_group = parser.add_mutually_exclusive_group()
        clear_me_group.add_argument(
            "--devices",
            "-d",
            help="removes all devices",
            action="store_true",
            default=False,
        )
        clear_me_group.add_argument(
            "--templates",
            "-t",
            help="removes all templates",
            action="store_true",
            default=False,
        )
        clear_me_group.add_argument(
            "--all",
            "-a",
            help="removes all templates and devices",
            action="store_true",
            default=False,
        )

    @staticmethod
    def config_dojot_create_parser(parser):
        """
        Configures the parser arguments for Dojot create parser.
        """
        parser.add_argument(
            "--devices",
            "-d",
            metavar="N",
            help="creates N devices in Dojot using a default template",
            type=int,
            required=True
        )
        parser.add_argument(
            "--batch",
            "-b",
            metavar="N",
            help="number of devices created per HTTP request, defaults to 100",
            type=int,
            default=100
        )

    @staticmethod
    def config_cert_parser(parser):
        """
        Configures the parser arguments for certificates parser.
        """
        create_me_group = parser.add_mutually_exclusive_group()
        create_me_group.add_argument(
            "--devices",
            metavar="N",
            help="number of certificates to be generated",
            type=int
        )
        create_me_group.add_argument(
            "--ids",
            metavar="ID",
            help="lists of IDs to generate the certificates",
            type=str,
            nargs="+"
        )
        create_me_group.add_argument(
            "--dojot",
            help="activates the generation of certificates for devices that are already in Dojot",
            action="store_true",
            default=False
        )

        parser.add_argument(
            "--processes",
            "-p",
            metavar="N",
            help=f"number of processes to generate the random certificates, defaults to the number \
                of cores in your machine, which is {os.cpu_count()}",
            type=int,
            default=os.cpu_count()
        )
        parser.add_argument(
            "--batch",
            "-b",
            metavar="N",
            help="prints the time spent to generate 'batch_size' certificates each time this number\
                of certificates is generated, defaults to 100",
            type=int,
            default=100
        )
        parser.add_argument(
            "--remove",
            "-r",
            help="activates the remotion of the existing certificates before the generation",
            action="store_true",
            default=False
        )

    @staticmethod
    def config_redis_parser(parser):
        """
        Configures the parser arguments for Redis parser.
        """
        parser.add_argument(
            "--map",
            "-m",
            help="applies the mapping of device IDs in Redis",
            action="store_true",
            default=False
        )
        parser.add_argument(
            "--restore",
            "-r",
            help="restores the Redis database to a fresh state,\
                without removing the certificates",
            action="store_true",
            default=False
        )
        parser.add_argument(
            "--clear",
            "-c",
            help="clears Redis, removing all certificates and mappings",
            action="store_true",
            default=False
        )
        parser.add_argument(
            "--export",
            "-e",
            help="exports the certificates",
            action="store_true"
        )


    ## Option execution ##
    def run(self):
        """
        Runs the commands for each parser.
        """
        if self.parser_args.topic == "cert":
            self.cert_commands()
        elif self.parser_args.topic == "dojot":
            if self.parser_args.dojot == "create":
                self.dojot_create_commands()
            if self.parser_args.dojot == "clear":
                self.dojot_clear_commands()
        elif self.parser_args.topic == "redis":
            self.redis_commands()

        if self.parser_args.topic is None:
            LOGGER.error('You must choose one command to run.')
            sys.exit(1)

    def cert_commands(self):
        """
        Certificate creation commands.
        """
        if self.parser_args.remove and os.path.exists(CONFIG['security']['cert_dir']):
            LOGGER.info("Removing certificates...")
            # We don't remove the cert directory because it can be mounted as a volume and it will
            # throw the 'Resource busy' error, we remove everything inside it instead
            for path in glob.glob(os.path.join(CONFIG['security']['cert_dir'], '*')):
                if os.path.isfile(path):
                    os.remove(path)
                if os.path.isdir(path):
                    shutil.rmtree(path)

            LOGGER.info("... Removed certificates")

            LOGGER.info("Creating certificates directories...")
            revoke_dir = os.path.join(
                CONFIG['security']['cert_dir'], CONFIG['security']['revoke_cert_dir']
            )
            renew_dir = os.path.join(
                CONFIG['security']['cert_dir'], CONFIG['security']['renew_cert_dir']
            )
            os.makedirs(renew_dir, exist_ok=True)
            os.makedirs(revoke_dir, exist_ok=True)
            LOGGER.info("... Created certificates directories")

        if self.parser_args.devices is not None:
            if self.parser_args.processes > self.parser_args.devices:
                LOGGER.error(
                    "The number of certificates must be greather than the number of processes!"
                )
                sys.exit(1)
            # Generating the random IDs
            ids = [str(uuid.uuid4().hex) for _ in range(self.parser_args.devices)]

            self.jwt = DojotAPI.get_jwt()
            # Begins the certificate generation for random devices IDs
            self.generate_certs(ids)

        elif self.parser_args.ids is not None:
            self.jwt = DojotAPI.get_jwt()
            # Begins the certificate generation
            self.generate_certs(self.parser_args.ids)

        elif self.parser_args.dojot:
            self.jwt = DojotAPI.get_jwt()
            devices_ids = DojotAPI.get_devices(self.jwt)
            self.generate_certs(devices_ids)

        else:
            LOGGER.error('You must choose one command to run.')
            sys.exit(1)

        # Exports the certificates' files
        self.export_certs()
        # Retrieving the CA certificate
        self.retrieve_ca_cert()
        # Mapping the certificates
        self.map_device_ids()

    def dojot_create_commands(self):
        """
        Dojot create commands execution.
        """
        self.create_devices()

    def dojot_clear_commands(self):
        """
        Dojot clear commands execution.
        """
        if self.parser_args.templates:
            self.jwt = DojotAPI.get_jwt()
            self.delete_templates()

        elif self.parser_args.devices:
            self.jwt = DojotAPI.get_jwt()
            self.delete_devices()

        elif self.parser_args.all:
            self.jwt = DojotAPI.get_jwt()
            self.delete_devices()
            self.delete_templates()

        else:
            LOGGER.error('You must choose one command to run.')
            sys.exit(1)

    def redis_commands(self):
        """
        Redis commands execution.
        """
        if self.parser_args.restore:
            self.restore_db_state()

        elif self.parser_args.clear:
            self.clear_db()
            self.restore_db_state()

        elif self.parser_args.map:
            self.map_device_ids()

        elif self.parser_args.export:
            # Retrieve JWT token
            self.jwt = DojotAPI.get_jwt()
            # Exports the certificates' files
            self.export_certs()
            # Retrieving the CA certificate
            self.retrieve_ca_cert()

        else:
            LOGGER.error('You must choose one command to run.')
            sys.exit(1)


    ## Commands for the options ##

    ## Dojot ##
    def create_devices(self):
        """
        Create the devices in Dojot.

        Returns a list with device IDs.
        """
        try:
            template_id = DojotAPI.create_template(self.jwt)
            DojotAPI.create_devices(
                self.jwt,
                template_id,
                self.parser_args.devices,
                self.parser_args.batch
            )

        except Exception as exception:
            LOGGER.error(str(exception))
            sys.exit(1)

    def delete_devices(self) -> None:
        """
        Deletes all devices.
        """
        DojotAPI.delete_devices(self.jwt)

    def delete_templates(self,) -> None:
        """
        Deletes all devices.
        """
        DojotAPI.delete_templates(self.jwt)


    ## Redis ##
    @staticmethod
    def connect_to_redis(database=CONFIG["locust"]["redis"]["certificates_db"]) -> \
                         redis.Redis:
        """
        Connects to Redis.

        Args:
            database: the database to be connected.
        """
        try:
            redis_conn = redis.Redis(
                host=CONFIG["locust"]["redis"]["host"],
                port=CONFIG["locust"]["redis"]["port"],
                db=database
            )
            redis_conn.ping()
            return redis_conn

        except Exception as exception:
            LOGGER.error("Error while connecting to Redis: %s", str(exception))
            sys.exit(1)

    def restore_db_state(self) -> None:
        """
        Restores the values of variables in the Redis database.
        """
        try:
            LOGGER.info("Restoring Redis database...")
            redis_conn = self.connect_to_redis(CONFIG["locust"]["redis"]["mapped_db"])

            redis_conn.set("devices_to_revoke", 0)
            redis_conn.set("devices_to_renew", 0)
            redis_conn.set("device_count", 0)
            redis_conn.delete("jwt")
            redis_conn.set("template_id", -1)

            redis_conn.save()

            LOGGER.info("... Redis databased successfully restored")
            redis_conn.close()

        except Exception as exception:
            LOGGER.error(str(exception))
            sys.exit(1)

    def clear_db(self) -> None:
        """
        Removes all entries in Redis databases.
        """
        try:
            LOGGER.info("Clearing Redis database...")
            redis_conn = self.connect_to_redis()
            redis_conn.flushall()
            LOGGER.info("... Redis databased successfully cleared")
            redis_conn.close()

        except Exception as exception:
            LOGGER.error(str(exception))
            sys.exit(1)

    def map_device_ids(self) -> None:
        """
        Maps device IDs from certificate database in Redis to sequential keys in mapping database.

        The certificates database stores the certificates and private keys  by using a Hash Map.
        When testing with Locust, we will read the already created certificates from Redis database,
        but since we are using Hash Maps, the retrieval is not trivial because we need to know the
        device ID.

        The solution is to create another database sequentially mapping the device IDs (i.e., the
        key will be an integer from 0 to the number of devices and the value is the device ID), so
        we can retrieve them safely.
        """
        cert_db = self.connect_to_redis()
        mapped_db = self.connect_to_redis(database=CONFIG["locust"]["redis"]["mapped_db"])

        try:
            LOGGER.info("Beginning database mapping...")

            keys = cert_db.keys()
            for i, key in enumerate(keys):
                mapped_db.set(i+1, key)

        except Exception as exception:
            LOGGER.error(str(exception))
            sys.exit(1)

        else:
            LOGGER.info("... Finished database mapping.")

        cert_db.close()
        mapped_db.close()

    def export_certs(self) -> None:
        """
        Creates the .crt and .key files from the generated certificates.
        """

        redis_conn = self.connect_to_redis()

        os.makedirs(CONFIG["security"]["cert_dir"], exist_ok=True)

        # Exporting .key files
        try:
            LOGGER.info("Creating key files...")
            for key in redis_conn.scan_iter(count=2):
                private_key = (redis_conn.hmget(name=key, keys="private_key"))
                filename = f"{CONFIG['security']['cert_dir']}/{key.decode('utf-8')}.key"

                with open(filename, "w") as key_file:
                    key_file.write((private_key[0]).decode("utf-8"))

        except Exception as exception:
            LOGGER.error("Error while saving key files: %s", str(exception))
            sys.exit(1)
        else:
            LOGGER.info("Successfully created!")


        # Exporting .crt files
        try:
            LOGGER.info("Creating certificate files...")
            for key in redis_conn.scan_iter(count=2):
                thing_certificate = (redis_conn.hmget(name=key, keys="thing_certificate"))
                filename = f"{CONFIG['security']['cert_dir']}/{key.decode('utf-8')}.crt"

                with open(filename, "w") as crt_file:
                    crt_file.write((thing_certificate[0]).decode("utf-8"))

        except Exception as exception:
            LOGGER.error("Error while saving key files: %s", str(exception))
            sys.exit(1)
        else:
            LOGGER.info("Successfully created!")

        redis_conn.close()

    def retrieve_ca_cert(self) -> None:
        """
        Retrieves the CA certificate and exports to a file.
        """
        certificate = DojotAPI.retrieve_ca_cert(self.jwt)

        filename = f"{CONFIG['security']['cert_dir']}{CONFIG['security']['ca_cert_file']}"
        with open(filename, "w") as ca_file:
            ca_file.write(certificate)

    def generate_certs(self, ids: list) -> None:
        """
        Wrapper for certificate generation functions.
        """
        LOGGER.info("Creating certificates...")
        processes = []
        id_list = self.calculate_process_load(self.parser_args.processes, ids)

        generated_queue = multiprocessing.Queue(self.parser_args.processes)
        saved_queue = multiprocessing.Queue(self.parser_args.processes)
        generated_certs = 0
        saved_certs = 0

        start = time.time()

        for i in range(self.parser_args.processes):
            proc = multiprocessing.Process(
                target=self.register_thing,
                args=(str(i), id_list[i], generated_queue, saved_queue)
            )
            proc.start()
            processes.append(proc)

        for i in range(self.parser_args.processes):
            processes[i].join()

        for i in range(self.parser_args.processes):
            generated_certs += generated_queue.get()
            saved_certs += saved_queue.get()

        if generated_certs != len(ids):
            LOGGER.error(
                "Failed to generate certificates, generated %i out of %i",
                generated_certs,
                len(ids)
            )
            sys.exit(1)
        if saved_certs != len(ids):
            LOGGER.error(
                "Failed to save certificates, saved %i out of %i",
                saved_certs,
                len(ids)
            )
            sys.exit(1)

        LOGGER.info(
            "Generated %i certificates and saved %i certificates in Redis in %is with %i processes",
            generated_certs,
            saved_certs,
            (time.time() - start),
            self.parser_args.processes
        )

    @staticmethod
    def calculate_process_load(processes: int, id_list: List[str]) -> \
                               Tuple[List[int], List[List[str]]]:
        """
        Calculates the processes' workloads by dividing them equally between each one.

        Parameters:
            processes: number of processes to divide de workload
            id_list: IDs to be divided in the processes

        Returns a list with lists of IDs to be generated by each process.
        """
        per_process = len(id_list) // processes
        exceeding = len(id_list) % processes

        workload = [per_process for _ in range(processes)]

        # Distribute the exceeding between the first processes
        for i in range(exceeding):
            workload[i] = workload[i] + 1

        ids_per_process = []
        prev = 0
        for load in workload:
            ids_per_process.append(id_list[prev:prev + load])
            prev += load

        return ids_per_process

    @staticmethod
    def execute_redis_pipe(pipe) -> int:
        """
        Executes the piped commands, handling any errors and retrying if necessary.

        Parameters:
            pipe: Redis pipeline object.

        Returns the number of certificates saved in Redis.
        """
        # Since the commands in the pipeline are only HMSET, it should not be a problem to
        # execute them again in the case of an error
        pipe_size = 0
        while True:
            try:
                pipe_size = len(pipe)
                pipe.execute()
            except Exception:
                LOGGER.warning(
                    "Something went wrong while saving certificates in Redis, trying again"
                )
                continue
            else:
                pipe.reset()
                return pipe_size

    def register_thing(
            self,
            name: str,
            id_list: List[str],
            generated_queue: multiprocessing.Queue,
            saved_queue: multiprocessing.Queue
    ) -> None:
        """
        Creates devices and exports them to a Redis database.

        Parameters:
            name: the process name.
            id_list: list of IDs to be used by the certificates.
            generated_queue: shared queue with the number of generated certificates in each process.
            saved_queue: shared queue with the number of certificates saved in Redis in each
            process.
        """
        redis_conn = self.connect_to_redis()

        # Disables atomicity to improve performance, since we don't need it at all
        pipe = redis_conn.pipeline(transaction=True)

        # Counters
        generated_certs = 0
        saved_certs = 0

        start_time = time.time()
        start_batch_time = start_time

        for i, thing_id in enumerate(id_list):

            if (i != 0) and ((i + 1) % self.parser_args.batch == 0):
                end_batch_time = time.time()
                LOGGER.info(
                    "Execution time: %f secs to generate %s certificates in process #%s, batch #%i",
                    end_batch_time - start_batch_time,
                    self.parser_args.batch,
                    name,
                    (i + 1) // self.parser_args.batch
                )

                saved_certs += self.execute_redis_pipe(pipe)

                start_batch_time = time.time()

            thing = None
            while True:
                try:
                    thing = Thing(CONFIG['app']['tenant'], thing_id)
                except Exception as exception:
                    LOGGER.error(str(exception))
                    LOGGER.info("Regenerating the certificate")
                    time.sleep(5)
                    continue
                else:
                    generated_certs += 1
                    break

            pipe.hmset(thing_id, thing.get_args_in_dict())

        saved_certs += self.execute_redis_pipe(pipe)

        generated_queue.put(generated_certs)
        saved_queue.put(saved_certs)

        end_time = time.time()
        redis_conn.close()
        LOGGER.info("Process %s finished in %fs", name, end_time - start_time)


if __name__ == "__main__":
    GenerateCerts()
