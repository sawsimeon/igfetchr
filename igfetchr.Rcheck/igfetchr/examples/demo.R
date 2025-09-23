## Demo script for igfetchr (offline/mock mode)
## This example is safe to run during CRAN checks — it uses the package's testing mode.

Sys.setenv(IGFETCHR_TESTING = "true")
library(igfetchr)

# Mock authentication
auth <- ig_auth("demo_user", "demo_pass", "demo_api_key", demo = TRUE)

# Mock market search
mock_markets <- data.frame(
  epic = c("TEST.EURUSD", "TEST.GBPUSD"),
  instrumentName = c("EUR/USD", "GBP/USD"),
  stringsAsFactors = FALSE
)
markets <- ig_search_markets("EUR/USD", auth = auth, mock_response = mock_markets)
print(markets)

# Mock current price
mock_price <- data.frame(bid = 1.1234, offer = 1.1238, timestamp = Sys.time(), stringsAsFactors = FALSE)
price <- ig_get_price("TEST.EURUSD", auth = auth, mock_response = mock_price)
print(price)

# Mock historical prices
mock_hist <- data.frame(
  snapshotTime = as.character(Sys.Date() - 2:0),
  open = c(1.12, 1.13, 1.11),
  high = c(1.14, 1.15, 1.12),
  low = c(1.11, 1.12, 1.09),
  close = c(1.13, 1.11, 1.10),
  stringsAsFactors = FALSE
)
hist <- ig_get_historical("TEST.EURUSD", from = Sys.Date()-2, to = Sys.Date(), resolution = "D", auth = auth, mock_response = mock_hist)
print(hist)

# Mock accounts
mock_accounts <- data.frame(accountId = "ACCT123", balance = 10000, stringsAsFactors = FALSE)
accounts <- ig_get_accounts(auth = auth, mock_response = mock_accounts)
print(accounts)

# Logout (mock)
ig_logout(auth)
Sys.unsetenv("IGFETCHR_TESTING")
