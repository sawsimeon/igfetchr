# Get historical prices for a market

Fetches historical prices for a market epic between specified dates at a
given resolution from the IG API. Uses the
/prices/{epic}/{resolution}/{startDate}/{endDate} endpoint (version 2)
with a fallback to version 3.

## Usage

``` r
ig_get_historical(
  epic,
  from,
  to,
  resolution = "D",
  page_size = 20,
  auth,
  mock_response = NULL,
  wait = 1
)
```

## Arguments

- epic:

  Character. Market epic (e.g., "CS.D.USDCHF.MINI.IP").

- from:

  Character or Date. Start date (e.g., "2025-09-01" or "2025-09-01
  00:00:00"). Required.

- to:

  Character or Date. End date (e.g., "2025-09-28" or "2025-09-28
  23:59:59"). Required.

- resolution:

  Character. Resolution code (e.g., "D", "1MIN", "HOUR"). Defaults to
  "D".

- page_size:

  Integer. Number of data points per page (v3 only). Defaults to 20.

- auth:

  List. Authentication details from \`ig_auth()\`, including \`cst\`,
  \`security\`, \`base_url\`, \`api_key\`, and \`acc_number\`.

- mock_response:

  List or data frame. Optional mock response for testing, bypassing the
  API call.

- wait:

  Numeric. Seconds to wait between paginated API calls (v3 only).
  Defaults to 1.

## Value

A tibble with historical OHLC data including snapshot time, bid, as,
open, close, high, low prices and last traded volume.

## Examples

``` r
if (FALSE) { # \dontrun{
# Authenticate and get historical prices
auth <- ig_auth(
  username = "your_username",
  password = "your_password",
  api_key = "your_api_key",
  acc_type = "DEMO",
  acc_number = "ABC123"
)
hist <- ig_get_historical(
  "CS.D.USDCHF.MINI.IP",
  from = "2025-09-01",
  to = "2025-09-28",
  resolution = "D",
  page_size = 20,
  auth
)
print(hist)

# Using time
hist <- ig_get_historical(
  "CS.D.USDCHF.MINI.IP",
  from = "2025-09-01 00:00:00",
  to = "2025-09-28 23:59:59",
  resolution = "D",
  page_size = 20,
  auth
)

# Using mock response
mock_response <- list(
  prices = data.frame(
    snapshotTime = "2025/09/01 00:00:00",
    openPrice.bid = 0.970,
    openPrice.ask = 0.971,
    openPrice.lastTraded = NA,
    highPrice.bid = 0.975,
    highPrice.ask = 0.976,
    highPrice.lastTraded = NA,
    lowPrice.bid = 0.965,
    lowPrice.ask = 0.966,
    lowPrice.lastTraded = NA,
    closePrice.bid = 0.971,
    closePrice.ask = 0.972,
    closePrice.lastTraded = NA,
    lastTradedVolume = 50000
  ),
  metadata = list(
    allowance = list(remainingAllowance = 10000),
    pageData = list(pageNumber = 1, totalPages = 1)
  )
)
hist <- ig_get_historical(
  "CS.D.USDCHF.MINI.IP",
  from = "2025-09-01",
  to = "2025-09-28",
  resolution = "D",
  page_size = 20,
  auth,
  mock_response = mock_response
)
} # }
```
