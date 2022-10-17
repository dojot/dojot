//go:generate ../../../tools/readme_config_includer/generator
package dojot_organization_creator

import (
	"bytes"
	"context"
	_ "embed"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"

	"github.com/influxdata/telegraf"
	httpconfig "github.com/influxdata/telegraf/plugins/common/http"
	"github.com/influxdata/telegraf/plugins/outputs"
	"github.com/influxdata/telegraf/plugins/serializers"
)

//go:embed sample.conf
var sampleConfig string

const (
	maxErrMsgLen = 1024
	defaultURL   = "http://127.0.0.1:8080/telegraf"
)

const (
	defaultContentType = "application/json"
	defaultMethod      = http.MethodPost
)

type DojotOrganizationCreator struct {
	URL                     string `toml:"url"`
	Method                  string `toml:"method"`
	Token                   string `toml:"token"`
	ContentEncoding         string `toml:"content_encoding"`
	OrganizationField       string `toml:"organization_field"`
	Bucket                  string `toml:"bucket"`
	NonRetryableStatuscodes []int  `toml:"non_retryable_statuscodes"`
	httpconfig.HTTPClientConfig
	Log telegraf.Logger `toml:"-"`

	client     *http.Client
	serializer serializers.Serializer
}

type InfluxBodyRequest interface {
}

type NewOrganizationBodyRequest struct {
	Name string `json:"name"`
}

type NewBucketBodyRequest struct {
	Name  string `json:"name"`
	OrgID string `json:"orgID"`
}

type NewOrganizationBodyResponse struct {
	Id string `json:"id"`
}

func (*DojotOrganizationCreator) SampleConfig() string {
	return sampleConfig
}

func (c *DojotOrganizationCreator) SetSerializer(serializer serializers.Serializer) {
	c.serializer = serializer
}

func (c *DojotOrganizationCreator) Connect() error {
	if c.Method == "" {
		c.Method = http.MethodPost
	}
	c.Method = strings.ToUpper(c.Method)

	ctx := context.Background()
	client, err := c.HTTPClientConfig.CreateClient(ctx, c.Log)
	if err != nil {
		return err
	}

	c.client = client

	return nil
}

func (c *DojotOrganizationCreator) Close() error {
	return nil
}

func (c *DojotOrganizationCreator) Write(metrics []telegraf.Metric) error {
	for _, metric := range metrics {
		org, ok := metric.GetField(c.OrganizationField)
		if !ok {
			return errors.New("Organization field is not defined")
		}

		newOrganizationBodyRequest := NewOrganizationBodyRequest{
			Name: fmt.Sprintf("%v", org),
		}

		orgID, err := c.createOrg(newOrganizationBodyRequest)
		if err != nil {
			return err
		}

		newBucketBodyRequest := NewBucketBodyRequest{
			Name:  "devices",
			OrgID: orgID,
		}

		err = c.createBucket(newBucketBodyRequest)
		if err != nil {
			return err
		}
	}

	return nil
}

func (c *DojotOrganizationCreator) createDefaultRequest(bodyRequest InfluxBodyRequest, url string) (*http.Request, error) {
	reqBody, err := json.Marshal(bodyRequest)
	if err != nil {
		return nil, err
	}
	reqBodyBuffer := bytes.NewBuffer(reqBody)
	request, err := http.NewRequest(c.Method, c.URL+url, reqBodyBuffer)
	if err != nil {
		return nil, err
	}

	request.Header.Set("Content-Type", defaultContentType)
	request.Header.Set("Authorization", c.Token)

	return request, nil
}

func (c *DojotOrganizationCreator) doRequest(request *http.Request) (*http.Response, error) {
	resp, err := c.client.Do(request)
	if err != nil {
		return nil, err
	}

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		for _, code := range c.NonRetryableStatuscodes {
			if code == resp.StatusCode {
				return resp, nil
			}
		}
		return nil, fmt.Errorf("when writing to [%s] received status code: %d", c.URL, resp.StatusCode)
	}

	return resp, nil
}

func (c *DojotOrganizationCreator) createOrg(bodyRequest NewOrganizationBodyRequest) (string, error) {
	newOrganizationReq, err := c.createDefaultRequest(bodyRequest, "/api/v2/orgs")
	if err != nil {
		return "", err
	}

	resp, err := c.doRequest(newOrganizationReq)
	if err != nil {
		return "", err
	}

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	orgBodyResponse := NewOrganizationBodyResponse{}
	err = json.Unmarshal(respBody, &orgBodyResponse)
	if err != nil {
		return "", err
	}

	return orgBodyResponse.Id, nil
}

func (c *DojotOrganizationCreator) createBucket(bodyRequest NewBucketBodyRequest) error {
	newBucketReq, err := c.createDefaultRequest(bodyRequest, "/api/v2/buckets")
	if err != nil {
		return err
	}

	_, err = c.doRequest(newBucketReq)
	if err != nil {
		return err
	}

	return nil
}

func init() {
	outputs.Add("dojot_organization_creator", func() telegraf.Output {
		return &DojotOrganizationCreator{
			Method: defaultMethod,
			URL:    defaultURL,
		}
	})
}
