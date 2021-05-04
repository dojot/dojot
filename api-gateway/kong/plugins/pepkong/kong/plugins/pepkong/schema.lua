local typedefs = require "kong.db.schema.typedefs"

local plugin_name = ({...})[1]:match("^kong%.plugins%.([^%.]+)")

return {
  name = plugin_name,
  fields = {
    { config = {
        type = "record",
        fields = {
          { resource = { type = "string", required = true, default = "Default Resource" }, },
          { scopes = {
            type = "map",
            keys = { type = "string" },
            values = { type = "string" },
            required = true,
            default = {
              ["GET"] = "view",
              ["POST"] = "create",
              ["PUT"] = "update",
              ["PATCH"] = "update",
              ["DELETE"] = "delete",
            }
         }}
        },
      },
    },
  },
}