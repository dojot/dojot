local build_form_params = require("kong.plugins.pepkong.utils").build_form_params

describe("Utility functions validation", function()

    it("Should build form parameters correctly", function()
        local resource = "my-resource"
        local scope = "view"
        local result = "grant_type=urn:ietf:params:oauth:grant-type:uma-ticket&audience=kong&permission=" .. resource ..
                           "%23" .. scope .. "&response_mode=decision"
        assert.same(result, build_form_params(resource, scope))
    end)

end)
