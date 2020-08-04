#!/bin/bash

# redirect stderr to stdout
exec 2>&1

# -e            Will make the whole script exit if a command fails.
#               Append '|| true' if you have commands that can fail without being an issue.
# -o pipefail   Causes a pipeline to produce a failure return code if any command errors.
set -eo pipefail

# DEBUG
if [ -n "${DEBUG+x}" ] ; then
    set -x
    #set -v
fi

readonly RED='\e[91m'
readonly CYAN='\e[96m'
readonly GREEN='\e[92m'
readonly YELLOW='\e[33m'
readonly NC='\e[39m' # Default foreground color
readonly BOLD='\e[1m'
readonly NRML='\e[0m'

readonly CA_DIR="${PWD}/ca"
readonly CERTS_DIR="${PWD}/certs"

readonly CA_CERT="${CA_DIR}/ca.crt"
readonly CA_KEY="${CA_DIR}/ca.key"
readonly CA_CNAME="${CA_CNAME:-"Root CA dummy (dojot IoT Platform)"}"
readonly CA_VALIDITY=${CA_VALIDITY:-1825} # in days
readonly HOST_VALIDITY=${HOST_VALIDITY:-365} # in days

function main() {

  echo '=================================================='
  echo 'This is a tool for generating certificates on disk'
  echo 'for end-to-end testing and local development.'
  echo '=================================================='

  precondition

  parseArgs "$@"

  createRootCACert

  createHostCert
}

function createRootCACert() {

  # Creates the CA directory if it does not exist
  [ ! -d "${CA_DIR}" ] && mkdir "${CA_DIR}"

  # Creates the CA certificate if it does not exist
  if [ ! -f "${CA_CERT}" ] && [ ! -f "${CA_KEY}" ]; then

    echo 'Creating the CA cert Private/Public Keys...'
    openssl ecparam -genkey -name 'secp521r1' -noout -out "${CA_KEY}"

    echo 'Creating a self-signed certificate for the root CA...'
    openssl req -x509 -new -nodes -key "${CA_KEY}" -sha256 -days "${CA_VALIDITY}" -out "${CA_CERT}" \
      -subj "/CN=${CA_CNAME}" \
      -config <(sed "s/RANDFILE\s*=\s*\$ENV::HOME\/\.rnd/#/" < '/etc/ssl/openssl.cnf')
  fi
}

function createHostCert() {

  local HOST_CSR="${CERTS_DIR}/${HOST_CNAME}.csr"
  local HOST_CERT="${CERTS_DIR}/${HOST_CNAME}.crt"
  local HOST_KEY="${CERTS_DIR}/${HOST_CNAME}.key"
  local HOST_EXT="${CERTS_DIR}/${HOST_CNAME}.ext"
  local SAN_EXT=""
  local HOST_SAN_DNS=""
  local HOST_SAN_IP=""

  # Creates the host certificate directory if it does not exist
  [ ! -d "${CERTS_DIR}" ] && mkdir "${CERTS_DIR}"

  # Creates the host certificate if it does not exist
  if [ ! -f "${HOST_CERT}" ] && [ ! -f "${HOST_KEY}" ]; then

    # splits the multi-valued DNS extension
    if [ -n "${HOST_DNS+x}" ]; then

      #Split the string based on the delimiter ','
      readarray -d , -t strarr <<< "${HOST_DNS}"

      # Print each value of the array by using loop
      for (( n=0; n < ${#strarr[*]}; n++)); do
        HOST_SAN_DNS="${HOST_SAN_DNS}DNS.$((n+1)) = ${strarr[n]}\n"
      done

      HOST_SAN_DNS=$(echo -e "${HOST_SAN_DNS}")
    fi

    # splits the multi-valued IP extension
    if [ -n "${HOST_IP+x}" ]; then

      #Split the string based on the delimiter ','
      readarray -d , -t strarr <<< "${HOST_IP}"

      # Print each value of the array by using loop
      for (( n=0; n < ${#strarr[*]}; n++)); do
        HOST_SAN_IP="${HOST_SAN_IP}IP.$((n+1)) = ${strarr[n]}\n"
      done

      HOST_SAN_IP=$(echo -e "${HOST_SAN_IP}")
    fi

    # If there is DNS or IP, we can define the SAN extension in the certificate
    if [ -n "${HOST_DNS+x}" ] || [ -n "${HOST_IP+x}" ]; then
      SAN_EXT="
subjectAltName = @alt_names
[alt_names]
${HOST_SAN_DNS}
${HOST_SAN_IP}
"
    fi

    # creates a temporary certificate extensions file
    cat > "${HOST_EXT}" << EOF
subjectKeyIdentifier = hash
authorityKeyIdentifier = keyid, issuer
basicConstraints = critical, CA:FALSE
keyUsage = digitalSignature, nonRepudiation, keyEncipherment, keyAgreement
extendedKeyUsage = serverAuth, clientAuth
${SAN_EXT}
EOF

    echo 'Creating the host cert public/private Keys...'
    openssl ecparam -genkey -name 'secp521r1' -noout -out "${HOST_KEY}"

    echo "Creating a CSR for the host '${HOST_CNAME}'..."
    openssl req -new -key "${HOST_KEY}" -out "${HOST_CSR}" -sha256 -subj "/CN=${HOST_CNAME}" \
      -config <(sed "s/RANDFILE\s*=\s*\$ENV::HOME\/\.rnd/#/" < '/etc/ssl/openssl.cnf')

    echo "Issuing a certificate to the host '${HOST_CNAME}'..."
    openssl x509 -req -CA "${CA_CERT}" -CAkey "${CA_KEY}" -CAcreateserial \
      -in "${HOST_CSR}" -out "${HOST_CERT}" -days "${HOST_VALIDITY}" -sha256 \
      -extfile "${HOST_EXT}"

    # removes temporary files
    rm "${HOST_EXT}" "${HOST_CSR}"

    echo -e "\xF0\x9F\x91\x8D The certificate for host '${BOLD}${HOST_CNAME}${NRML}' has been ${GREEN}successfully generated${NC}!"
  else
    echo -e "\xF0\x9F\x91\x8E A certificate ${RED}already exists${NC} for the host '${BOLD}${HOST_CNAME}${NRML}'!"
  fi
}

function parseArgs() {
  # positional args
  local args=()

  # named args
  while [ "$1" != "" ]; do
    case "$1" in
      --cname ) HOST_CNAME="$2"; shift;;
      --ip ) HOST_IP="$2"; shift;;
      --dns ) HOST_DNS="$2"; shift;;
      * )  args+=("$1")  # if no match, add it to the positional args
    esac
    shift # move to next key-value pair
  done

  # restore positional args
  set -- "${args[@]}"

  readonly HOST_CNAME=${HOST_CNAME:-"localhost"}
  readonly HOST_IP
  readonly HOST_DNS
}

function precondition() {
    checker "OpenSSL" "openssl" "openssl"
}

function checker() {
  local programName=$1
  local commandName=$2
  local aptPkgName=$3

  echo -e "Checking if ${YELLOW}${programName}${NRML}${NC} are installed..."
  command -v "${commandName}" >/dev/null 2>&1 || {
    echo -e >&2 "${RED}Error - ${BOLD}${programName}${NRML} isn't installed."
    echo -e >&2 "${NC}To install ${BOLD}${programName}${NRML} run: ${CYAN}sudo apt install ${aptPkgName}"
    echo -e >&2 "${RED}Aborting...${NC}"
    exit 1
  }
  echo -e "${BOLD}${programName}${NRML} ${GREEN}checked${NC}!"
}

main "$@";