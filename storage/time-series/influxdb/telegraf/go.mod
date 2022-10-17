module github.com/influxdata/telegraf

go 1.19

require (
	cloud.google.com/go/bigquery v1.40.0
	cloud.google.com/go/monitoring v1.5.0
	cloud.google.com/go/pubsub v1.25.1
	cloud.google.com/go/storage v1.23.0
	collectd.org v0.5.0
	github.com/Azure/azure-event-hubs-go/v3 v3.3.18
	github.com/Azure/azure-kusto-go v0.8.0
	github.com/Azure/azure-storage-queue-go v0.0.0-20191125232315-636801874cdd
	github.com/Azure/go-autorest/autorest v0.11.28
	github.com/Azure/go-autorest/autorest/adal v0.9.21
	github.com/Azure/go-autorest/autorest/azure/auth v0.5.11
	github.com/BurntSushi/toml v1.2.0
	github.com/ClickHouse/clickhouse-go v1.5.4
	github.com/DATA-DOG/go-sqlmock v1.5.0
	github.com/Masterminds/sprig v2.22.0+incompatible
	github.com/Mellanox/rdmamap v0.0.0-20191106181932-7c3c4763a6ee
	github.com/Shopify/sarama v1.36.0
	github.com/aerospike/aerospike-client-go/v5 v5.10.0
	github.com/alecthomas/units v0.0.0-20210208195552-ff826a37aa15
	github.com/aliyun/alibaba-cloud-sdk-go v1.61.1727
	github.com/amir/raidman v0.0.0-20170415203553-1ccc43bfb9c9
	github.com/antchfx/jsonquery v1.3.0
	github.com/antchfx/xmlquery v1.3.12
	github.com/antchfx/xpath v1.2.1
	github.com/apache/iotdb-client-go v0.12.2-0.20220722111104-cd17da295b46
	github.com/apache/thrift v0.16.0
	github.com/aristanetworks/goarista v0.0.0-20190325233358-a123909ec740
	github.com/armon/go-socks5 v0.0.0-20160902184237-e75332964ef5
	github.com/aws/aws-sdk-go-v2 v1.16.14
	github.com/aws/aws-sdk-go-v2/config v1.15.7
	github.com/aws/aws-sdk-go-v2/credentials v1.12.5
	github.com/aws/aws-sdk-go-v2/feature/ec2/imds v1.12.13
	github.com/aws/aws-sdk-go-v2/service/cloudwatch v1.19.1
	github.com/aws/aws-sdk-go-v2/service/cloudwatchlogs v1.15.13
	github.com/aws/aws-sdk-go-v2/service/dynamodb v1.16.1
	github.com/aws/aws-sdk-go-v2/service/ec2 v1.54.4
	github.com/aws/aws-sdk-go-v2/service/kinesis v1.15.10
	github.com/aws/aws-sdk-go-v2/service/sts v1.16.13
	github.com/aws/aws-sdk-go-v2/service/timestreamwrite v1.13.12
	github.com/aws/smithy-go v1.13.3
	github.com/benbjohnson/clock v1.3.0
	github.com/blues/jsonata-go v1.5.4
	github.com/bmatcuk/doublestar/v3 v3.0.0
	github.com/caio/go-tdigest v3.1.0+incompatible
	github.com/cisco-ie/nx-telemetry-proto v0.0.0-20220628142927-f4160bcb943c
	github.com/coocood/freecache v1.2.2
	github.com/coreos/go-semver v0.3.0
	github.com/coreos/go-systemd v0.0.0-20190719114852-fd7a80b32e1f
	github.com/couchbase/go-couchbase v0.1.1
	github.com/denisenkom/go-mssqldb v0.12.0
	github.com/dimchansky/utfbom v1.1.1
	github.com/djherbis/times v1.5.0
	github.com/docker/docker v20.10.17+incompatible
	github.com/docker/go-connections v0.4.0
	github.com/doclambda/protobufquery v0.0.0-20220727165953-0da287796ee9
	github.com/dynatrace-oss/dynatrace-metric-utils-go v0.5.0
	github.com/eclipse/paho.golang v0.10.0
	github.com/eclipse/paho.mqtt.golang v1.4.1
	github.com/fatih/color v1.13.0
	github.com/go-ldap/ldap/v3 v3.4.4
	github.com/go-logfmt/logfmt v0.5.1
	github.com/go-redis/redis/v7 v7.4.1
	github.com/go-redis/redis/v8 v8.11.5
	github.com/go-sql-driver/mysql v1.6.0
	github.com/go-stomp/stomp v2.1.4+incompatible
	github.com/gobwas/glob v0.2.3
	github.com/gofrs/uuid v4.2.0+incompatible
	github.com/golang-jwt/jwt/v4 v4.4.2
	github.com/golang/geo v0.0.0-20190916061304-5b978397cfec
	github.com/golang/snappy v0.0.4
	github.com/google/gnxi v0.0.0-20220411075422-cd6b043b7fd0
	github.com/google/go-cmp v0.5.9
	github.com/google/go-github/v32 v32.1.0
	github.com/google/licensecheck v0.3.1
	github.com/google/uuid v1.3.0
	github.com/gopcua/opcua v0.3.3
	github.com/gophercloud/gophercloud v1.0.0
	github.com/gorilla/mux v1.8.0
	github.com/gorilla/websocket v1.5.0
	github.com/gosnmp/gosnmp v1.34.0
	github.com/grid-x/modbus v0.0.0-20211113184042-7f2251c342c9
	github.com/gwos/tcg/sdk v0.0.0-20220621192633-df0eac0a1a4c
	github.com/harlow/kinesis-consumer v0.3.6-0.20211204214318-c2b9f79d7ab6
	github.com/hashicorp/consul/api v1.14.0
	github.com/hashicorp/go-uuid v1.0.3
	github.com/influxdata/go-syslog/v3 v3.0.0
	github.com/influxdata/influxdb-observability/common v0.2.22
	github.com/influxdata/influxdb-observability/influx2otel v0.2.21
	github.com/influxdata/influxdb-observability/otel2influx v0.2.22
	github.com/influxdata/line-protocol/v2 v2.2.1
	github.com/influxdata/tail v1.0.1-0.20210707231403-b283181d1fa7
	github.com/influxdata/toml v0.0.0-20190415235208-270119a8ce65
	github.com/influxdata/wlog v0.0.0-20160411224016-7c63b0a71ef8
	github.com/intel/iaevents v1.0.0
	github.com/jackc/pgconn v1.13.0
	github.com/jackc/pgio v1.0.0
	github.com/jackc/pgtype v1.12.0
	github.com/jackc/pgx/v4 v4.17.1
	github.com/james4k/rcon v0.0.0-20120923215419-8fbb8268b60a
	github.com/jhump/protoreflect v1.8.3-0.20210616212123-6cc1efa697ca
	github.com/jmespath/go-jmespath v0.4.0
	github.com/kardianos/service v1.2.1
	github.com/karrick/godirwalk v1.17.0
	github.com/kballard/go-shellquote v0.0.0-20180428030007-95032a82bc51
	github.com/kolo/xmlrpc v0.0.0-20201022064351-38db28db192b
	github.com/lxc/lxd v0.0.0-20220920163450-e9b4b514106a
	github.com/matttproud/golang_protobuf_extensions v1.0.2-0.20181231171920-c182affec369
	github.com/mdlayher/apcupsd v0.0.0-20220319200143-473c7b5f3c6a
	github.com/microsoft/ApplicationInsights-Go v0.4.4
	github.com/miekg/dns v1.1.50
	github.com/moby/ipvs v1.0.2
	github.com/multiplay/go-ts3 v1.0.1
	github.com/nats-io/nats-server/v2 v2.8.4
	github.com/nats-io/nats.go v1.17.0
	github.com/newrelic/newrelic-telemetry-sdk-go v0.8.1
	github.com/nsqio/go-nsq v1.1.0
	github.com/olivere/elastic v6.2.37+incompatible
	github.com/openconfig/gnmi v0.0.0-20200617225440-d2b4e6a45802
	github.com/opentracing/opentracing-go v1.2.0
	github.com/openzipkin-contrib/zipkin-go-opentracing v0.4.5
	github.com/openzipkin/zipkin-go v0.2.5
	github.com/pborman/ansi v1.0.0
	github.com/pion/dtls/v2 v2.1.5
	github.com/pkg/errors v0.9.1
	github.com/prometheus-community/pro-bing v0.1.0
	github.com/prometheus/client_golang v1.13.0
	github.com/prometheus/client_model v0.2.0
	github.com/prometheus/common v0.37.0
	github.com/prometheus/procfs v0.8.0
	github.com/prometheus/prometheus v1.8.2-0.20210430082741-2a4b8e12bbf2
	github.com/rabbitmq/amqp091-go v1.5.0
	github.com/riemann/riemann-go-client v0.5.1-0.20211206220514-f58f10cdce16
	github.com/robbiet480/go.nut v0.0.0-20220219091450-bd8f121e1fa1
	github.com/safchain/ethtool v0.0.0-20210803160452-9aa261dae9b1
	github.com/sensu/sensu-go/api/core/v2 v2.14.0
	github.com/shirou/gopsutil/v3 v3.22.8
	github.com/showwin/speedtest-go v1.1.5
	github.com/signalfx/golib/v3 v3.3.45
	github.com/sirupsen/logrus v1.9.0
	github.com/sleepinggenius2/gosmi v0.4.4
	github.com/snowflakedb/gosnowflake v1.6.2
	github.com/stretchr/testify v1.8.0
	github.com/tbrandon/mbserver v0.0.0-20170611213546-993e1772cc62
	github.com/testcontainers/testcontainers-go v0.13.0
	github.com/tidwall/gjson v1.14.3
	github.com/tinylib/msgp v1.1.6
	github.com/urfave/cli/v2 v2.16.3
	github.com/vapourismo/knx-go v0.0.0-20220829185957-fb5458a5389d
	github.com/vjeantet/grok v1.0.1
	github.com/vmware/govmomi v0.28.1-0.20220921224932-b4b508abf208
	github.com/wavefronthq/wavefront-sdk-go v0.10.4
	github.com/wvanbergen/kafka v0.0.0-20171203153745-e2edea948ddf
	github.com/xdg/scram v1.0.5
	github.com/yuin/goldmark v1.4.13
	go.mongodb.org/mongo-driver v1.10.2
	go.opentelemetry.io/collector/pdata v0.56.0
	go.opentelemetry.io/otel/exporters/otlp/otlpmetric/otlpmetricgrpc v0.31.0
	go.opentelemetry.io/otel/metric v0.31.0
	go.opentelemetry.io/otel/sdk/metric v0.31.0
	go.starlark.net v0.0.0-20220328144851-d1966c6b9fcd
	golang.org/x/mod v0.6.0-dev.0.20220419223038-86c51ed26bb4
	golang.org/x/net v0.0.0-20220909164309-bea034e7d591
	golang.org/x/oauth2 v0.0.0-20220822191816-0ebed06d0094
	golang.org/x/sync v0.0.0-20220907140024-f12130a52804
	golang.org/x/sys v0.0.0-20220919091848-fb04ddd9f9c8
	golang.org/x/text v0.3.7
	golang.zx2c4.com/wireguard/wgctrl v0.0.0-20211230205640-daad0b7ba671
	gonum.org/v1/gonum v0.12.0
	google.golang.org/api v0.94.0
	google.golang.org/genproto v0.0.0-20220916172020-2692e8806bfa
	google.golang.org/grpc v1.49.0
	google.golang.org/protobuf v1.28.1
	gopkg.in/gorethink/gorethink.v3 v3.0.5
	gopkg.in/olivere/elastic.v5 v5.0.86
	gopkg.in/tomb.v1 v1.0.0-20141024135613-dd632973f1e7
	gopkg.in/yaml.v2 v2.4.0
	k8s.io/api v0.25.0
	k8s.io/apimachinery v0.25.1
	k8s.io/client-go v0.25.0
	modernc.org/sqlite v1.17.3
)

require (
	code.cloudfoundry.org/clock v1.0.0 // indirect
	github.com/Masterminds/goutils v1.1.1 // indirect
	github.com/apache/arrow/go/arrow v0.0.0-20211006091945-a69884db78f4 // indirect
	github.com/aristanetworks/glog v0.0.0-20191112221043-67e8567f59f3 // indirect
	github.com/aws/aws-sdk-go-v2/feature/s3/manager v1.5.3 // indirect
	github.com/bitly/go-hostpool v0.1.0 // indirect
	github.com/containerd/cgroups v1.0.4 // indirect
	github.com/containerd/containerd v1.6.7 // indirect
	github.com/containerd/continuity v0.3.0 // indirect
	github.com/couchbase/gomemcached v0.1.3 // indirect
	github.com/couchbase/goutils v0.1.0 // indirect
	github.com/docker/distribution v2.8.1+incompatible // indirect
	github.com/goburrow/modbus v0.1.0 // indirect
	github.com/goburrow/serial v0.1.1-0.20211022031912-bfb69110f8dd // indirect
	github.com/grid-x/serial v0.0.0-20211107191517-583c7356b3aa // indirect
	github.com/huandu/xstrings v1.3.2 // indirect
	github.com/mdlayher/genetlink v1.2.0 // indirect
	github.com/mitchellh/copystructure v1.2.0 // indirect
	github.com/moby/sys/mount v0.3.3 // indirect
	github.com/naoina/go-stringutil v0.1.0 // indirect
	github.com/opencontainers/runc v1.1.3 // indirect
	github.com/pierrec/lz4 v2.6.1+incompatible // indirect
	github.com/pkg/browser v0.0.0-20210911075715-681adbf594b8 // indirect
	github.com/samuel/go-zookeeper v0.0.0-20200724154423-2164a8ac840e // indirect
	github.com/wvanbergen/kazoo-go v0.0.0-20180202103751-f72d8611297a // indirect
	github.com/youmark/pkcs8 v0.0.0-20201027041543-1326539a0a0a // indirect
	golang.zx2c4.com/wireguard v0.0.0-20211209221555-9c9e7e272434 // indirect
	gopkg.in/fatih/pool.v2 v2.0.0 // indirect
	gotest.tools v2.2.0+incompatible
	k8s.io/kube-openapi v0.0.0-20220803164354-a70c9af30aea // indirect
)
