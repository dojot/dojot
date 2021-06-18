local BasePlugin = require "kong.plugins.base_plugin"
local jwt_decoder = require "kong.plugins.jwt.jwt_parser"
local http = require "resty.http"

local build_form_params = require("kong.plugins.pepkong.utils").build_form_params

local re_gmatch = ngx.re.gmatch

----------- Load environment variables ---------------

local env_ssl_verify = "DOJOT_PLUGIN_SSL_VERIFY"
local env_request_timeout = "DOJOT_PLUGIN_REQUEST_TIMEOUT"


local ssl_verify = true
if (os.getenv(env_ssl_verify) and  string.lower(os.getenv(env_ssl_verify)) == "false" ) then
    ssl_verify = false
end

local request_timeout = 500
if(os.getenv(env_request_timeout)) then
    request_timeout = tonumber(os.getenv(env_request_timeout))
end

-------------------------------------------------------

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

    kong.log.debug('Invoke PDP/Keycloak at ', token_endpoint, ' with: ')
    kong.log.debug(' params: ', params)
    kong.log.debug(' ssl_verify: ', ssl_verify)
    kong.log.debug(' request_timeout: ', request_timeout)

    local httpc = http.new()

    if(request_timeout) then
        httpc:set_timeout(request_timeout)
    end

    -- to use ssl_verify=true it is necessary to set the variable lua_ssl_trusted_certificate
    local res, err = httpc:request_uri(token_endpoint,  {
        method = "POST",
        ssl_verify = ssl_verify,
        body = params,
        headers = {
            ["Authorization"] = "Bearer " .. token,
            ["Content-Type"] = "application/x-www-form-urlencoded",
            ["Accept"] = 'application/json',
            ["content-length"] = string.len(params)
        },
    } )


    if not res then
        kong.log.debug('Error ',500,' ', tostring(err))
        return false, {
            status = 500,
            message = tostring(err)
        }
    end


    if res.status ~= 200 then
        kong.log.debug('Error ',res.status,' ', res.body)
        return false, {
            status = res.status,
            message = res.body
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
