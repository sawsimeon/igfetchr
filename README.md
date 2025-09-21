# igfetchr

igfetchr is a lightweight R wrapper for the IG Trading REST API (labs.ig.com). It provides beginner-friendly functions to fetch market data, search historical prices, execute trades, and get account details from the IG Trading REST API (labs.ig.com) — returning tidy tibbles for easy analysis.

Important: Trading CFDs and spread bets carries a high risk of losing money. This package not financial advice.

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
auth <- igfetchr::ig_auth("user", "pass", "api_key", demo = TRUE)

# Use mock_response to simulate endpoints
markets <- igfetchr::ig_search_markets("EUR/USD", auth = auth,
                                      mock_response = data.frame(epic = "TEST.EURUSD", name = "EUR/USD"))

print(markets)

# Clear testing mode
Sys.unsetenv("IGFETCHR_TESTING")
```

## Vignette

See the vignette `vignettes/getting-started.Rmd` for a short guided demo that uses mock responses so it runs offline during CRAN checks.

## Contributing

Please open issues or pull requests on the GitHub repository: https://github.com/sawsimeon/igfetchr

## License

MIT License — see LICENSE file.
