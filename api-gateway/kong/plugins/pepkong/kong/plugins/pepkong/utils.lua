local function build_form_params(resource, scope)

    local grant_type = "urn:ietf:params:oauth:grant-type:uma-ticket"

    -- define client identifier
    local client_id_env_var = "DOJOT_PLUGIN_CLIENT_ID"
    local client_id = "kong"
    if os.getenv(client_id_env_var) then
        client_id = os.getenv(client_id_env_var)
    end

    return "grant_type=" .. grant_type .. "&audience=" .. client_id .. "&permission=" .. resource .. "%23" .. scope ..
               "&response_mode=decision"
end

return {
    build_form_params = build_form_params
}
