//go:build !custom || outputs || outputs.influxdb_v2

package all

import _ "github.com/influxdata/telegraf/plugins/outputs/dojot_organization_creator" // register plugin
