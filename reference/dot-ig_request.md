# Internal function to make HTTP requests to IG API

Internal function to make HTTP requests to IG API

## Usage

``` r
.ig_request(
  path,
  auth,
  method = c("GET", "POST", "PUT", "DELETE"),
  query = list(),
  body = NULL,
  version = NULL,
  mock_response = NULL
)
```

## Arguments

- path:

  Character. API endpoint path (e.g., "/positions/otc").

- auth:

  List. Authentication details from ig_auth().

- method:

  Character. HTTP method ("GET", "POST", "PUT", "DELETE").

- query:

  List. Query parameters for GET requests. Defaults to list().

- body:

  List. Request body for POST or PUT requests. Defaults to NULL.

- version:

  Character. API version ("1", "2", "3"). Defaults to NULL.

- mock_response:

  List or data frame. Optional mock response for testing, bypassing the
  API call.

## Value

List with API response (status code and body) or tibble if mock_response
is a data frame.
