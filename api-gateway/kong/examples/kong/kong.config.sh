#!/bin/sh

kong="http://apigw:8001"
dojot_domain_name=${DOJOT_DOMAIN_NAME:-localhost}

# check if kong is started
if curl --output /dev/null --silent --head --fail "$kong"; then
  echo ""
  echo "Kong is started."
else
  echo "Kong isn't started."
  echo "Terminating in 20s..."
  sleep 20
  exit 1
fi

# add authentication to an endpoint
addAuthToEndpoint() {
# $1 = Service Name
echo ""
echo ""
echo "- addAuthToEndpoint: ServiceName=${1}"

# To understand about lua patterns and its limitations http://lua-users.org/wiki/PatternsTutorial
# This regex is to guarantee whether or not it has port 443 and 80, it will be able to validate the issuer
allowed_iss_url="https?://${dojot_domain_name}:?(%d*)/auth/realms/(.+)"

curl -X POST ${kong}/services/"${1}"/plugins \
  --data "name=jwt-keycloak" \
  --data-urlencode "config.allowed_iss=${allowed_iss_url}"

curl  -sS  -X POST \
--url ${kong}/services/"${1}"/plugins/ \
--data "name=pepkong" \
--data "config.resource=${1}"

}

# add a Service
# that is the name Kong uses to refer to the upstream APIs
# and microservices it manages.
createService() {
# $1 = Service Name
# $2 = URL (ex.: http://gui:80)
echo ""
echo "-- createService: ServiceName=${1} Url=${2}"
curl  -sS -X PUT \
--url ${kong}/services/"${1}" \
--data "name=${1}" \
--data "url=${2}"
}

# add a Route
# The Route represents the actual request to the Kong proxy
# endpoint to reach at Kong service.
createRoute() {
# $1 = Service Name
# $2 = Route Name
# $3 = PATHS (ex.: '"/","/x"')
# $4 = strip_path (true or false), When matching a Route via one of the paths, strip the matching prefix from the upstream request URL
echo ""
echo "-- createRoute: ServiceName=${1} Url=${2} PathS=${3} StripPath=${4}"
(curl  ${kong}/services/"${1}"/routes/"${2}" -sS -X PUT \
    --header "Content-Type: application/json" \
    -d @- ) <<PAYLOAD
{
    "paths": [${3}],
    "strip_path": ${4}
}
PAYLOAD
}

# Create an endpoint mapping in Kong
# ex1: createEndpoint "data-broker" "http://data-broker:80"  '"/device/(.*)/latest", "/subscription"' "false"
# ex2: createEndpoint "image" "http://image-manager:5000"  '"/fw-image"' "true"
createEndpoint(){
# $1 = Service Name
# $2 = URL (ex.: "http://gui:80")
# $3 = PATHS (ex.: '"/","/x"')
# $4 = strip_path ("true" or "false"), When matching a Route via one of the paths, strip the matching prefix from the upstream request URL.
echo ""
echo ""
echo "- createEndpoint: ServiceName=${1} Url=${2} PathS=${3} StripPath=${4}"
createService "${1}" "${2}"
createRoute "${1}" "${1}_route" "${3}" "${4}"
}

createEndpoint "server-api-example-sec" "http://server-api-example:8888"  '"/secure"' "false"
addAuthToEndpoint "server-api-example-sec"

createEndpoint "server-api-example-insec" "http://server-api-example:8888"  '"/insecure"' "false"

createEndpoint "keycloak" "http://keycloak:8080/auth"  '"/auth"' "true"


echo ""
echo ""
echo "- add plugin rate-limiting in keycloak"
curl  -s  -sS -X POST \
--url ${kong}/services/auth-service/plugins/ \
--data "name=rate-limiting" \
--data "config.minute=5" \
--data "config.hour=40" \
--data "config.policy=local"

