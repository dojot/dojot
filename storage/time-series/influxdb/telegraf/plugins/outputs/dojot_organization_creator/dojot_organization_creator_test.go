package dojot_organization_creator

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/influxdata/telegraf"
	"github.com/influxdata/telegraf/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var (
	dojotOrganizationCreator = &DojotOrganizationCreator{
		URL:               "http://localhost:9999",
		OrganizationField: "tenant",
		Method:            "POST",
	}
)

func TestConnect(t *testing.T) {
	err := dojotOrganizationCreator.Connect()

	require.NoError(t, err, "It should not return an error")
	require.NotNil(t, dojotOrganizationCreator.client, "It should fill http client")
}

func TestWrite(t *testing.T) {
	ts := httptest.NewServer(
		http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "application/json")
			switch r.URL.Path {
			case "/api/v2/orgs":
				w.WriteHeader(http.StatusCreated)
				json.NewEncoder(w).Encode(struct {
					OrgID string `json:"orgID"`
				}{
					OrgID: "loklerg",
				})
				return
			case "/api/v2/buckets":
				w.WriteHeader(http.StatusCreated)
				return
			default:
				w.WriteHeader(http.StatusNotFound)
				return
			}
		}),
	)
	defer ts.Close()
	dojotOrganizationCreator.URL = "http://" + ts.Listener.Addr().String()

	// Data
	fields := make(map[string]interface{})
	fields["tenant"] = "test_tenant"
	metrics := []telegraf.Metric{
		testutil.MustMetric("tenant", make(map[string]string), fields, time.Now(), telegraf.Histogram),
	}

	// Testes
	dojotOrganizationCreator.Connect()
	err := dojotOrganizationCreator.Write(metrics)
	require.NoError(t, err, "it should not return an error")

	dojotOrganizationCreator.URL = "http://" + ts.Listener.Addr().String() + "/error"
	err = dojotOrganizationCreator.Write(metrics)
	require.Error(
		t,
		err,
		"It should return an error when the request failed",
	)

	dojotOrganizationCreator.NonRetryableStatuscodes = append(dojotOrganizationCreator.NonRetryableStatuscodes, 404)
	err = dojotOrganizationCreator.Write(metrics)
	require.Error(
		t,
		err,
		"It shouldn't return an error when the request failed with a code that exist in non-repeatable status codes",
	)

	dojotOrganizationCreator.OrganizationField = ""
	err = dojotOrganizationCreator.Write(metrics)
	assert.Equal(
		t,
		err.Error(),
		"Organization field is not defined", "It should return an error when the organization field is not defined",
	)

}
