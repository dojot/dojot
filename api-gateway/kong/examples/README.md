# Example

This example is intended to deploy an example using Kong with the `pep-kong` and `jwt-keycloak` plugins and a secure endpoint using a JWT provided by Keycloak.

__ATTENTION__ It's highly recommended to communicate with Keycloak over HTTPS.


To run this example, type:

```sh
docker-compose up
```

# Using the example

As prerequisites this uses curl and jq .

On Debian-based Linux distributions, you can install these prerequisites by running:

```sh
sudo apt install curl jq
```

## Obtaining Access Token

As `user`:

```sh
JWT=$(curl --location --request POST http://localhost:8000/auth/realms/admin/protocol/openid-connect/token \
--data-urlencode 'username=user' \
--data-urlencode 'password=user' \
--data-urlencode 'client_id=cli' \
--data-urlencode 'grant_type=password' 2>/dev/null | jq -r '.access_token')
```

As `admin`:

```sh
JWT=$(curl --location --request POST http://localhost:8000/auth/realms/admin/protocol/openid-connect/token \
--data-urlencode 'username=admin' \
--data-urlencode 'password=admin' \
--data-urlencode 'client_id=cli' \
--data-urlencode 'grant_type=password' 2>/dev/null | jq -r '.access_token')
```

## Checking if it was defined in the JWT variable

```sh
echo $JWT
```

## Accessing resource/endpoint

```sh
curl -X GET "http://localhost:8000/secure" -H  "Authorization: Bearer ${JWT}"
```

```sh
curl -X DELETE "http://localhost:8000/secure" -H  "Authorization: Bearer ${JWT}"
```

```sh
curl -X GET "http://localhost:8000/insecure"
```

# Kong

## Kong settings

In the `kong/kong.config.sh` file are created configurations of routes, services and associations with the kong plugins (pep-kong and jwt-keycloak) for each registered service

## Konga (Kong GUI)

To access the Konga (Kong interface) use http://localhost:1337.

# Keycloak

To access the Admin Keycloak interface use http://localhost:8000/auth and use the login `admin` and password `admin`.

## Keycloak settings

When starting the keycloak, a Realm called **admin** is imported with the configurations listed below:

### Roles settings

In Roles on the left sidebar:

- Create a new Role with the name 'user'

- Create a new Role with the name 'admin'
  - Enable the `Composite Roles` option
  - In `Available Roles` select `user` and `Add selected`, so that `user` will be listed in `Associated Roles`.

### User settings depends on ‘Role settings’

Users in the left sidebar:

- I created a new user with username `admin` that will have *role*  `admin`
  - Enable the `Email Verified` option (for local testing only)
  - Go to the `Credentials` tab
    - Define a password and confirm it in `Password` and`Password Confirmation`, in the example the password `admin` is used (Only for local testing)
    - Disable the `Temporary` option (for local testing only)
  - Go to the `Role Mappings` tab
    - In `Available Roles` select `admin` and `Add selected`, so that `admin` will be listed in `Associated Roles`.

- I created a new user with username `user` that will have *role* `user`
  - Enable the `Email Verified` option (for local testing only)
  - Go to the `Credentials` tab
    - Define a password and confirm it in `Password` and`Password Confirmation`, in the example the password `user` is used (Only for local testing)
    - Enable the `Temporary` option (for local testing only)
  - Go to the `Role Mappings` tab
    - In `Available Roles` select`user` and `Add selected`, so that`user` will be listed in `Associated Roles`.

### Kong client settings depends on ‘roles settings’

Clients in the left sidebar:

- Create a new `client` with the *Client ID* as`kong`
  - Enable the `Enable` option if it is not enabled
  - Set `Client Protocol` to`openid-connect`
  - Set `Access Type` to`confidential`
  - Enable the `Standard Flow Enabled` option if it is not enabled
  - Disable the `Implicit Flow Enabled` option if it is enabled
  - Enable the `Direct Access Grants Enabled` option if it is not enabled
  - Enable the `Service Accounts Enabled` option if it is not enabled
  - Enable the option `Authorization Enabled` if it is not enabled
  - Set `Valid Redirect URIs` to` http://localhost:8000/* `

#### Configuring authorization

- Go to the `Authorization` tab
  - Within `Authorization` select the`Authorization Scopes` tab
    - Create the following scopes: `create`,`delete`, `update` and`view`.
      - Note: **`create`  is for HTTP**POST**,  `delete` is for HTTP **DELETE**,    `update`is for HTTP **PUT** or **PATCH** and `view` is for HTTP **GET**.
  - Within `Authorization` select the`Polices` tab
    - Create a policy for role `admin`
      - In `Create Policy` select`role`
        - Set to name `Should be admin`
        - Set the `admin` value in`Realm Roles` and select it as `Required`
        - Set `Logic` to` Positive
    - Create a policy for role `user`
      - In `Create Policy` select`role`
        - Set to name `Should be user
        - Set the `user` value in`Realm Roles` and select it as `Required`
        - Set `Logic` to`Positive`

In the following steps It will configure a resource called `server-api-example-sec` in which a user with the role `admin` can create, edit and delete, while a user with the role user can only view:

- Within `Authorization` select the`Resources` tab
  - Create a new resource for the example "server-api-example-sec"
    - Set `name` to`server-api-example-sec`*
    - Set some significant value to identify in `Display name`, in this case something like`Server Api Example with security`
    - In `scopes` set the values`create`, `delete`,`update` and `view`
    - Enable `User-Managed Access Enabled`
    - Click save
- Within `Authorization` select the`Permissions` tab
  - In `Create Permission` select`Scope-based`
    - Define a meaningful `name` as in this case`Modifications Server Api Example with security`
    - In `resource` define`server-api-example-sec`
    - In `scopes` set the values`create`, `delete` and`update`
    - In `Apply Policy` set`Should be admin`
    - In `Decision Strategy` leave`Unanimous`
  - In `Create Permission` select`Scope-based`
    - Define a meaningful `name` as in this case`View Server Api Example with security`
    - In `resource` define`server-api-example-sec`
    - In `scopes` define the values`view`
    - In `Apply Policy` set`Should be user`
    - In `Decision Strategy` leave`Unanimous`

Client cli settings depends on ‘kong client settings’

- Create a new `client` with *Client ID* as`cli`
  - Enable the `Enable` option if it is not enabled
  - Set `Client Protocol` to`openid-connect`
  - Set `Access Type` to`public`
  - Disable the `Standard Flow Enabled` option if it is enabled
  - Disable the `Implicit Flow Enabled` option if it is enabled
  - Enable the `Direct Access Grants Enabled` option if it is not enabled
  - Go to the `Scope` tab
    - Enable `Full Scope Allowed` or select the`scope` `user`

Client gui settings depends on ‘kong client settings’

- Create a new `client` with *Client ID* as`gui`
  - Enable the `Enable` option if it is not enabled
  - Set `Client Protocol` to`openid-connect`
  - Set `Access Type` to`public`
  - Enable the `Standard Flow Enabled` option if it is enabled
  - Disable the `Implicit Flow Enabled` option if it is enabled
  - Enable the `Direct Access Grants Enabled` option if it is not enabled
  - Set `Valid Redirect URIs` to `http://localhost:8000/*`
  - Go to the `Scope tab
    - Enable `Full Scope Allowed` or select the`scope` `user`

## Checking settings

### Checking permissions that the user has

```sh
curl -X POST \
http://localhost:8000/auth/realms/admin/protocol/openid-connect/token \
 -H "Authorization: Bearer ${JWT}" \
 --data "grant_type=urn:ietf:params:oauth:grant-type:uma-ticket" \
 --data "audience=kong" \
 --data "response_mode=permissions"
```

### Checking if the user can access the resource they are trying to access (This is similar request that the pep kong plugin makes)

```sh
curl -X POST \
http://localhost:8000/auth/realms/admin/protocol/openid-connect/token \
 -H "Authorization: Bearer ${JWT}" \
 --data "grant_type=urn:ietf:params:oauth:grant-type:uma-ticket" \
 --data "audience=kong" \
 --data "response_mode=decision" \
 --data "permission=server-api-example-sec#view"

```
