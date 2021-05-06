#!/bin/bash

# redirect stderr to stdout
exec 2>&1

# -e            Will make the whole script exit if a command fails.
#               Append '|| true' if you have commands that can fail without being an issue.
# -o pipefail   Causes a pipeline to produce a failure return code if any command errors.
#set -eo pipefail

# DEBUG
if [ -n "${DEBUG+x}" ] ; then
  set -x
  #set -v
fi

# default values (can be changed by parameters)
HOST="127.0.0.1"
PORT="8000"
TENANT="admin"
USERNAME="admin"
PASSWD="admin"
DEVICE_IDS=''
JWT=''
TOKEN_FILE=''

readonly KEY_PAIR_FILE='private.key'
readonly CSR_FILE='request.csr'
readonly CA_FILE='ca.pem'
readonly CERT_FILE='cert.pem'

readonly RED='\e[91m'
readonly CYAN='\e[96m'
readonly NC='\e[39m' # Default foreground color
readonly BOLD='\e[1m'
readonly NRML='\e[0m'

function main() {

  echo '============================================================='
  echo 'This is a tool to request dojot certificates for IoT devices.'
  echo '============================================================='

  precondition

  parseArgs "$@"

  if [ -z "${JWT}" ]; then
    getToken
  fi

  if [ -z "${DEVICE_IDS}" ]; then
    getDeviceIDs
  fi

  getTrustedCA

  getCertificates
}

function getToken() {
  printf '\xE2\x8F\xB3 Obtaining access token...'

  JWT=$(curl -sS -X POST "${HOST}:${PORT}/auth/realms/${TENANT}/protocol/openid-connect/token" \
        --data-urlencode "username=${USERNAME}" \
        --data-urlencode "password=${PASSWD}" \
        --data-urlencode "client_id=cli" \
        --data-urlencode "grant_type=password" 2>/dev/null \
    | jq -j '.access_token')

  if [ -z "${JWT}" ]; then
    printf '\r\xE2\x9D\x8C Failed to get access token!\n'
    exit 1
  else
    echo "${JWT}" > "${TOKEN_FILE}"
    printf '\r\xE2\x9C\x94 Obtained access token!    \n'
  fi
}

function getDeviceIDs() {
  printf '\xE2\x8F\xB3 Obtaining device IDs...'

  DEVICE_IDS=$(curl -sS -X GET "${HOST}:${PORT}/device?idsOnly=true" \
    -H "Authorization: Bearer ${JWT}" \
    -H 'Content-Type: application/json' \
    | jq -r '.[]?')

  if [ -z "${DEVICE_IDS}" ]; then
    printf '\r\xE2\x9D\x8C Failed to get device IDs!\n'
    exit 1
  else
    printf '\r\xE2\x9C\x94 Obtained device IDs!    \n'
  fi
}

function getTrustedCA() {
  printf '\xE2\x8F\xB3 Obtaining the certificate from the trusted CA...'

  local dir
  local trustedCA

  dir="./ca"
  [ ! -d "${dir}" ] && mkdir "${dir}"

  trustedCA=$(curl -sS -X GET "${HOST}:${PORT}/x509/v1/ca" \
    -H 'Content-Type:application/json' \
    -H "Authorization: Bearer ${JWT}" \
    -H 'Accept:application/json' | jq '.caPem' -r )

  echo "${trustedCA}" > "${dir}/${CA_FILE}"
  printf '\r\xE2\x9C\x94 Obtained the certificate from the trusted CA!    \n'
}

function getCertificates() {

  local dir
  local csrContent
  local cert

  for id in ${DEVICE_IDS}; do

    printf '\xE2\x8F\xB3 Obtaining the certificate for device "%s"...' "${id}"

    dir="./cert_${id}"
    [ ! -d "${dir}" ] && mkdir "${dir}"

    # Generating CSR and private key
    openssl req -newkey rsa:2048 -nodes -sha256 \
      -keyout "${dir}/${KEY_PAIR_FILE}" -out "${dir}/${CSR_FILE}" \
      -subj "/CN=${id}" \
      2>/dev/null

    # Reading the CSR file
    csrContent=$(while IFS= read -r line; do printf '%s\\n' "$line"; done <"${dir}/${CSR_FILE}")
    csrContent=${csrContent:0:(-2)}

    # Requesting the certificate
    cert=$(curl -sS -X POST "${HOST}:${PORT}/x509/v1/certificates" \
      -H 'Content-Type:application/json' \
      -H "Authorization: Bearer ${JWT}" \
      -H 'Accept:application/json' \
      --data-binary "{ \"csr\": \"${csrContent}\" }" | jq '.certificatePem' -r )

    echo "${cert}" > "${dir}/${CERT_FILE}"
    printf '\r\xE2\x9C\x94 Obtained the certificate for device "%s"!    \n' "${id}"

    # removes temporary CSR
    rm "${dir}/${CSR_FILE}"
  done
}

function parseArgs() {
  # positional args
  local args=()

  # named args
  while [ "$1" != "" ]; do
    case "$1" in
      -h ) HOST="${2}"; shift;;
      -p ) PORT="${2}"; shift;;
      -r ) TENANT="${2}"; shift;;
      -i ) DEVICE_IDS="$2"; shift;;
      -u ) USERNAME="${2}"; shift;;
      -s ) PASSWD="${2}"; shift;;
      -t ) TOKEN_FILE="${2}"; shift;;
      * )  args+=("$1")  # if no match, add it to the positional args
    esac
    shift # move to next key-value pair
  done

  # restore positional args
  set -- "${args[@]}"

  # split a comma separated deviceIDs string
  if [ -n "${DEVICE_IDS+x}" ] ; then
    DEVICE_IDS=$(echo -e "${DEVICE_IDS//,/'\n'}")
  fi

  # If the name of the file containing the access token is not entered
  # by parameter, then a default name is defined...
  if [ -z "${TOKEN_FILE}" ] ; then
    TOKEN_FILE='token.jwt'
  fi

  # If the file containing the access token exists, then the token is loaded
  # into the variable. This way, it is not necessary to use the username and
  # password to generate a new token.
  if [ -f "${TOKEN_FILE}" ]; then
    JWT=$(cat "${TOKEN_FILE}")
  fi

  readonly HOST
  readonly PORT
  readonly TENANT
  readonly USERNAME
  readonly PASSWD
  readonly TOKEN_FILE
}

function precondition() {
    checker "cURL" "curl" "curl"
    checker "OpenSSL" "openssl" "openssl"
    checker "jq" "jq" "jq"
}

function checker() {
  local programName=$1
  local commandName=$2
  local aptPkgName=$3

  command -v "${commandName}" >/dev/null 2>&1 || {
    echo -e >&2 "${RED}Error - ${BOLD}${programName}${NRML} isn't installed."
    echo -e >&2 "${NC}To install ${BOLD}${programName}${NRML} run: ${CYAN}sudo apt install ${aptPkgName}"
    echo -e >&2 "${RED}Aborting...${NC}"
    exit 1
  }
}

main "$@";
