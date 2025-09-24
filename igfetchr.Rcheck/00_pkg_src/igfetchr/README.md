# igfetchr

igfetchr is a lightweight R wrapper for the IG Trading REST API (labs.ig.com). It provides beginner-friendly functions to fetch market data, search historical prices, execute trades, and get account details from the IG Trading REST API (labs.ig.com) — returning tidy tibbles for easy analysis.

Important: Trading CFDs and spread bets carries a high risk of losing money. This package is not financial advice.

## Installation

Install the package from GitHub (development):

```r
# install.packages("remotes")
remotes::install_github("sawsimeon/igfetchr")
```

## Quick example (mocked / offline)

The package supports an offline "testing" mode so examples and tests run without network calls. Set the environment variable `IGFETCHR_TESTING = "true"` to enable mock behavior.

```r
Sys.setenv(IGFETCHR_TESTING = "true")

# Mock authentication (returns mock tokens)
auth <- igfetchr::ig_auth(
  username = "demo_user",
  password = "demo_pass",
  api_key = "demo_api_key",
  acc_type = "DEMO",
  acc_number = "ABC123"
)

# Use mock_response to simulate endpoints
markets <- igfetchr::ig_search_markets(
  search_term = "USD/CHF",
  auth = auth,
  mock_response = data.frame(
    epic = "CS.D.USDCHF.CFD.IP",
    instrumentName = "USD/CHF",
    stringsAsFactors = FALSE
  )
)

print(markets)

# Clear testing mode
Sys.unsetenv("IGFETCHR_TESTING")

```

## Vignette

See the vignette `vignettes/getting-started.Rmd` for a short guided demo that uses mock responses so it runs offline during CRAN checks.

## Real API Usage

For live API calls, set environment variables for security and use `ig_auth()` without testing mode.

```r
Sys.setenv(IG_SERVICE_USERNAME = "your_username")
Sys.setenv(IG_SERVICE_PASSWORD = "your_password")
Sys.setenv(IG_SERVICE_API_KEY = "your_api_key")
Sys.setenv(IG_SERVICE_ACC_TYPE = "DEMO")
Sys.setenv(IG_SERVICE_ACC_NUMBER = "ABC123")

auth <- igfetchr::ig_auth()
hist <- igfetchr::ig_get_historical(
  epic = "CS.D.USDCHF.CFD.IP",
  from = "2020-01-01",
  to = "2020-12-31",
  resolution = "D",
  auth = auth
)
print(hist)
```

## Contributing

Please open issues or pull requests on the GitHub repository: https://github.com/sawsimeon/igfetchr

## License

MIT License — see `LICENSE` file.
