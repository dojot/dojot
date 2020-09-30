# **Scripts**

To facilitate some jobs, we provide scripts that automates tasks. As for now, there is only one
script: the `generate_certs`.

# `generate_certs`

The main goal of this script is to facilitate the certificate generation. It also includes dojot
functionalities, such as device and template creation/removal.

**Did you run into a problem while running this script? Check the [troubleshooting](#troubleshooting)
section!**

## **Prerequisites**

Before running this script, you must ensure that there is a dojot and a Locust Master instances
running. For more details on running a dojot instance, check the
[official documentation](https://dojotdocs.readthedocs.io/en/stable/).

If what you need is to only generate certificates from the dojot instance you are running to use in
other tests that will not use Locust, you can initialize a standalone Redis container and point to
it in the `REDIS_HOST` environment variable.

## **A note about certificate storage**

Whenever you want to backup the certificates that you've created, you don't need to copy the `cert`
directory, simply copy the `db/dump.rdb` file to your backup location.

If you want to restore a Redis database that already has certificates in it, simply copy the dump to
the `db` directory.

## **Running**

Assuming you are in the main Locust directory, simply run the command:
```shell
docker-compose -f Docker/scripts/generate_certs/docker-compose.yml up -d
```

After its initialization, enter in the container:
```shell
docker-compose -f Docker/scripts/generate_certs/docker-compose.yml exec generate-certs bash
```

Now that you are inside the container, the script can be run by executing:
```shell
generate_certs
```

__NOTE THAT__ this is an alias to the actual command:
```shell
python -m src.scripts.generate_certs
```

## **Commands**

The script accepts 3 sub-commands:

- `cert`: responsible for the certificate generation.
- `dojot`: responsible for device/template operations.
- `redis`: responsible for certificate information manipulation in Redis.

For more information on which parameters each sub-command accepts, run it with the `-h` or `--help`
flags.

## **Usage examples**

**1. I want to create 100 virtual devices.**

Simply run:
```shell
generate_certs cert --devices 100
```

**2. I want to create 100 devices in the dojot platform and retrieve their certificates.**

Creating the devices:
```shell
generate_certs dojot create --devices 100
```

Generating their certificates:
```shell
generate_certs cert --dojot
```

**3. The last example created certificates for all the devices I had in my dojot instance. Is there
a way to generate the certificates for only the devices with IDs `123abc`, `456def` and `789ghi`?**

Yes, there is. Simply run:
```shell
generate_certs cert --ids 123abc,456def,789ghi
```

**4. I want to export all the certificates that are stored in the current Redis database.**

Simply run:
```shell
generate_certs redis --export
```

This is important when using more than one machine to run Locust: the Locust master centralizes the
certificates in its Redis database and distribute them to other computers by simply pointing them to
its Redis instance and running the aforementioned command.

## Troubleshooting

Here are some common problems that can happen when running the `generate_certs` script. If you find
anything that is not in here, feel free to open an issue telling us what is the problem!

**1. When I run the `generate_certs` command, a MonkeyPatchWarning message is issued at the
beginning. Is this a problem?**

This is a warning that this Locust version throws. This does not cause any harm as we could see and
will be corrected when we update its version.

**2. A `502: Bad Gateway` message has appeared when generating certificates, but the program didn't
crash. Is this a problem?**

Sometimes, the number of requests to the Dojot API coming from the `generate_certs` script can be too
much for the default dojot configuration. The script has defensive code to prevent this error from
crashing the program, but it will report whenever it occurs. If for any reason this problem crashes
the script, you can reduce the generation batch size (option `--batch` in the `cert` sub-command)
and/or increase the wait time between batches (option `--wait` in the `cert` sub-command).
