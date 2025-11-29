# Search markets

Search markets by text query. Returns a tibble of matching markets from
the IG API.

## Usage

``` r
ig_search_markets(query, auth, mock_response = NULL)
```

## Arguments

- query:

  Character. Search string for markets (e.g., "USD/CHF").

- auth:

  List. Authentication details from \`ig_auth()\`, including \`cst\`,
  \`security\`, \`base_url\`, \`api_key\`, and \`acc_number\`.

- mock_response:

  List or data frame. Optional mock response for testing, bypassing the
  API call.

## Value

A tibble with market information including epic, instrument name and
market status.

## Examples

``` r
if (FALSE) { # \dontrun{
# Authenticate and search markets
auth <- ig_auth(
  username = "your_username",
  password = "your_password",
  api_key = "your_api_key",
  acc_type = "DEMO",
  acc_number = "ABC123"
)
markets <- ig_search_markets("USD/CHF", auth)
print(markets)

# Using mock response
mock_response <- data.frame(
  epic = "CS.D.USDCHF.MINI.IP",
  instrumentName = "USD/CHF Mini",
  marketStatus = "OPEN"
)
markets <- ig_search_markets("USD/CHF", auth, mock_response = mock_response)
} # }
```
