package = "kong-plugin-pepkong"

version = "0.1.0-1"
-- The version '0.1.0' is the source code version, the trailing '1' is the version of this rockspec.
-- whenever the source version changes, the rockspec should be reset to 1. The rockspec version is only
-- updated (incremented) when this file changes, but the source remains the same.

local pluginName = package:match("^kong%-plugin%-(.+)$")

supported_platforms = {"linux", "macosx"}
source = {
  url = "https://github.com/dojot/dojot.git"
}

description = {
  summary = "A Kong plugin for implementing the PEP (Policy Enforcement Point)",
  detailed = [[
        A Kong plugin for implementing the PEP (Policy Enforcement Point).
        It was designed to integrate with Keycloak that implements the PDP (Policy Decision Point)
  ]],
  license = "Apache 2.0"
}

dependencies = {
  "lua ~> 5"
}

build = {
  type = "builtin",
  modules = {
    ["kong.plugins."..pluginName..".handler"] = "kong/plugins/"..pluginName.."/handler.lua",
    ["kong.plugins."..pluginName..".utils"] = "kong/plugins/"..pluginName.."/utils.lua",
    ["kong.plugins."..pluginName..".schema"] = "kong/plugins/"..pluginName.."/schema.lua",
  }
}
