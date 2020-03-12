#!/bin/sh
#
# command.sh
#
# The Dockerfile CMD, or any "docker run" command option, gets
# passed as command-line arguments to this script.

# Abort on any error (good shell hygiene)
set -e

# run what was given in the command
exec "$@"