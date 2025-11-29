# Get current price for a market

Fetches current price(s) for the given market epic from the IG API using
the \`/markets/{epic}\` endpoint.

## Usage

``` r
ig_get_price(epic, auth, mock_response = NULL)
```

## Arguments

- epic:

  Character. Market epic (e.g., "CS.D.USDCHF.CFD.IP").

- auth:

  List. Authentication details from \`ig_auth()\`, including \`cst\`,
  \`security\`, \`base_url\`, \`api_key\`, and \`acc_number\`.

- mock_response:

  List or data frame. Optional mock response for testing, bypassing the
  API call.

## Value

A tibble with price information including makret status, bid, offer,
high, low, and update time.

## Examples

``` r
if (FALSE) { # \dontrun{
# Authenticate and get price
auth <- ig_auth(
  username = "your_username",
  password = "your_password",
  api_key = "your_api_key",
  acc_type = "DEMO",
  acc_number = "ABC123"
)
price <- ig_get_price("CS.D.USDCHF.CFD.IP", auth)
print(price)

# Using mock response for testing
mock_response <- list(
  snapshot = data.frame(
    marketStatus = "TRADEABLE",
    bid = 0.798,
    offer = 0.798,
    high = 0.801,
    low = 0.797,
    updateTime = "2025/09/26 21:58:57",
    binaryOdds = NA
  )
)
price <- ig_get_price("CS.D.USDCHF.CFD.IP", auth, mock_response = mock_response)
} # }
```
