local BasePlugin = require "kong.plugins.base_plugin"
local jwt_decoder = require "kong.plugins.jwt.jwt_parser"
local http = require "socket.http"
local https = require "ssl.https"

local build_form_params = require("kong.plugins.pepkong.utils").build_form_params

local re_gmatch = ngx.re.gmatch

local env_ssl_ca_file = "DOJOT_PLUGIN_SSL_CAFILE"
local env_ssl_verify = "DOJOT_PLUGIN_SSL_VERIFY"
local env_ssl_cert_file = "DOJOT_PLUGIN_SSL_CERTFILE"
local env_ssl_key_file = "DOJOT_PLUGIN_SSL_KEYFILE"


local pepKongHandler = BasePlugin:extend()

function pepKongHandler:new()
    pepKongHandler.super.new(self, "pepkong")
end

local function retrieve_token()

    local authorization_header = kong.request.get_header("authorization")
    if authorization_header then
        local iterator, iter_err = re_gmatch(authorization_header, "\\s*[Bb]earer\\s+(.+)")
        if not iterator then
            return nil, iter_err
        end

        local m, err = iterator()
        if err then
            return nil, err
        end

        if m and #m > 0 then
            return m[1]
        end
    end
    return kong.response.exit(401, { message = "Missing JWT token" })
end

-- Use Keycloak authorization services to make authorization decision
local function do_authorization(conf)

    -- Retrieve token
    local token, err = retrieve_token()
    if err then
        kong.log.err(err)
        return kong.response.exit(500, {
            message = "An unexpected error occurred"
        })
    end

    -- Decode token
    local jwt, err = jwt_decoder:new(token)
    if err then
        return false, {
            status = 401,
            message = "Bad token; " .. tostring(err)
        }
    end

    -- Invoke PDP/Keycloak
    local params = build_form_params(conf.resource, conf.scopes[kong.request.get_method()])
    local token_endpoint = jwt.claims.iss .. "/protocol/openid-connect/token"

    local protocol = string.sub(token_endpoint, 0, 5)

    kong.log.debug('Invoke PDP/Keycloak in endpoint: ', token_endpoint)
    kong.log.debug('Invoke PDP/Keycloak with params: ', params)

    local response = {}

    local header_request = {
        ["Authorization"] = "Bearer " .. token,
        ["Content-Type"] = "application/x-www-form-urlencoded",
        ["Accept"] = 'application/json',
        ["content-length"] = string.len(params)
    }

    local source_request = ltn12.source.string(params)
    local method_request = "POST"
    local sink_request = ltn12.sink.table(response)

    local base_request = {
        method = method_request,
        url = token_endpoint,
        source = source_request,
        headers = header_request,
        sink = sink_request
    }

    local do_request = nil

    if protocol == "https" then
        local ssl_ca_file = os.getenv(env_ssl_ca_file)
        local ssl_cert_file = os.getenv(env_ssl_cert_file)
        local ssl_key_file = os.getenv(env_ssl_key_file)
        local ssl_verify = os.getenv(env_ssl_verify)

        if (ssl_ca_file) then
            base_request['cafile']=ssl_ca_file
        end

        if (ssl_cert_file) then
            base_request['certificate']=ssl_cert_file
        end

        if (ssl_key_file) then
            base_request['key']=ssl_key_file
        end

        if (ssl_verify) then
            base_request['verify']=ssl_verify
        else
            base_request['verify']="peer"
        end

        base_request['protocol']="any"

        base_request['mode']="client"

        base_request['options']= { "all",
            -- disable this protocols bellow
            "no_sslv2",
            "no_sslv3",
            "no_tlsv1",
            "no_tlsv1_1"}

        do_request = https.request

    else
        do_request = http.request
    end

    local body, code, headers, status = do_request (base_request)

    local message = response[1]

    if code ~= 200 then
        return false, {
            status = code,
            message = message
        }
    end

    return true

end

function pepKongHandler:access(conf)
    pepKongHandler.super.access(self)

    -- validate if the request is valid
    if not conf.scopes[kong.request.get_method()] then
        return kong.response.exit(405)
    end

    local ok, err = do_authorization(conf)
    if not ok then
        return kong.response.exit(err.status, err.message)
    end

end

return pepKongHandler
