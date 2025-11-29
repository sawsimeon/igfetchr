# Getting Started with igfetchr

## Introduction

igfetchr is a beginner-friendly wrapper around the IG Trading REST API
(labs.ig.com). It provides functions to authenticate, fetch markets,
current prices, historical prices, account summaries, execute trades,
and close sessions.

**Disclaimer:** Trading CFDs and spread bets carry a high risk of losing
money. The package igfetchr is not financial advice.

## Offline / testing mode

To make examples and tests deterministic and CRAN-friendly, igfetchr
supports an offline testing mode. Set the environment variable
`IGFETCHR_TESTING = "true"` to return mock tokens and accept
`mock_response` data frames for endpoints.

``` r
Sys.setenv(IGFETCHR_TESTING = "true")
library(igfetchr)
```

## Authenticate (mock)

This example uses the package’s testing mode and returns a mock auth
list instantly.

``` r
auth <- ig_auth(
  username = "demo_user",
  password = "demo_pass",
  api_key = "demo_api_key",
  acc_type = "DEMO",
  acc_number = "ABC123"
)
auth
#> $cst
#> [1] "mock_cst"
#> 
#> $security
#> [1] "mock_security"
#> 
#> $base_url
#> [1] "https://demo-api.ig.com/gateway/deal"
#> 
#> $api_key
#> [1] "demo_api_key"
#> 
#> $acc_type
#> [1] "DEMO"
#> 
#> $acc_number
#> [1] "ABC123"
```

## Search markets (mock)

Use
[`ig_search_markets()`](https://sawsimeon.github.io/igfetchr/reference/ig_search_markets.md)
with `mock_response` to simulate the API returning market results.

``` r
mock_markets <- data.frame(
  epic = c("CS.D.USDCHF.CFD.IP"),
  instrumentName = c("USD/CHF"),
  stringsAsFactors = FALSE
)

markets <- ig_search_markets("USD/CHF", auth = auth, mock_response = mock_markets)
#> Warning: Unknown or uninitialised column: `markets`.
markets
#> # A tibble: 0 × 2
#> # ℹ 2 variables: epic <chr>, name <chr>
```

## Current price (mock)

Simulate a current price response for USD/CHF.

``` r
mock_price <- data.frame(
  bid = 0.8500,
  offer = 0.8504,
  timestamp = Sys.time(),
  stringsAsFactors = FALSE
)
price <- ig_get_price("CS.D.USDCHF.CFD.IP", auth = auth, mock_response = mock_price)
#> Warning: Unknown or uninitialised column: `snapshot`.
price
#> # A tibble: 0 × 2
#> # ℹ 2 variables: bid <dbl>, offer <dbl>
```

## Historical prices (mock)

Simulate historical OHLC data for USD/CHF.

``` r
mock_hist <- data.frame(
  snapshotTime = as.character(Sys.Date() - 2:0),
  open = c(0.8500, 0.8550, 0.8520),
  high = c(0.8520, 0.8570, 0.8540),
  low = c(0.8480, 0.8530, 0.8500),
  close = c(0.8510, 0.8540, 0.8530),
  stringsAsFactors = FALSE
)

hist <- ig_get_historical(
  epic = "CS.D.USDCHF.CFD.IP",
  from = Sys.Date() - 2,
  to = Sys.Date(),
  resolution = "D",
  auth = auth,
  mock_response = mock_hist
)
#> Warning in ig_get_historical(epic = "CS.D.USDCHF.CFD.IP", from = Sys.Date() - :
#> Incompatible methods ("Ops.Date", "Ops.POSIXt") for ">"
#> Warning in ig_get_historical(epic = "CS.D.USDCHF.CFD.IP", from = Sys.Date() - :
#> Incompatible methods ("Ops.Date", "Ops.POSIXt") for ">"
#> Warning: Unknown or uninitialised column: `prices`.
#> No prices returned from API for epic 'CS.D.USDCHF.CFD.IP'. Verify epic and date range with IG support at labs.ig.com.
hist
#> # A tibble: 0 × 10
#> # ℹ 10 variables: snapshotTime <chr>, open_bid <dbl>, open_ask <dbl>,
#> #   close_bid <dbl>, close_ask <dbl>, high_bid <dbl>, high_ask <dbl>,
#> #   low_bid <dbl>, low_ask <dbl>, volume <int>
```

## Accounts (mock)

Simulate account summary retrieval.

``` r
mock_accounts <- data.frame(
  accountId = "ACCT123",
  balance = 10000,
  preferred = TRUE,
  stringsAsFactors = FALSE
)
accounts <- ig_get_accounts(auth = auth, mock_response = mock_accounts)
#> Warning: Unknown or uninitialised column: `accounts`.
#> No accounts returned from API. Verify authentication with IG support at labs.ig.com.
accounts
#> # A tibble: 0 × 13
#> # ℹ 13 variables: accountId <chr>, accountName <chr>, accountAlias <lgl>,
#> #   status <chr>, accountType <chr>, preferred <lgl>, currency <chr>,
#> #   canTransferFrom <lgl>, canTransferTo <lgl>, balance <dbl>, deposit <dbl>,
#> #   profitLoss <dbl>, available <dbl>
```

## Execute Trade (mock)

Simulate executing a trade for USD/CHF.

``` r
mock_trade <- data.frame(
  dealId = "DIXXXX",
  dealReference = "REF123",
  status = "OPEN",
  stringsAsFactors = FALSE
)
trade <- ig_execute_trade(
  epic = "CS.D.USDCHF.CFD.IP",
  direction = "BUY",
  size = 1.0,
  auth = auth,
  mock_response = mock_trade
)
trade
#> # A tibble: 1 × 3
#>   dealId dealReference status
#>   <chr>  <chr>         <chr> 
#> 1 DIXXXX REF123        OPEN
```

## Close Session (mock)

Close the session.

``` r
ig_close_session(auth, mock_response = TRUE)
Sys.unsetenv("IGFETCHR_TESTING")
```

## Notes

- The vignette uses the built-in testing mode so it runs quickly and
  without network, satisfying CRAN checks.
- Real API usage requires a free API key from <https://labs.ig.com> and
  valid credentials. Replace the mock usage above with real calls and
  remove the testing environment variable when running live.
