local BasePlugin = require "kong.plugins.base_plugin"
local jwt_decoder = require "kong.plugins.jwt.jwt_parser"
local http = require "socket.http"

local build_form_params = require("kong.plugins.pepkong.utils").build_form_params

local re_gmatch = ngx.re.gmatch

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

    kong.log.debug('Invoke PDP/Keycloak in endpoint: ', token_endpoint)
    kong.log.debug('Invoke PDP/Keycloak with params: ', params)

    local response = {}
    local body, code, headers, status = http.request {
        method = "POST",
        url = token_endpoint,
        source = ltn12.source.string(params),
        headers = {
            ["Authorization"] = "Bearer " .. token,
            ["Content-Type"] = "application/x-www-form-urlencoded",
            ["Accept"] = 'application/json',
            ["content-length"] = string.len(params)
        },
        sink = ltn12.sink.table(response)
    }

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
