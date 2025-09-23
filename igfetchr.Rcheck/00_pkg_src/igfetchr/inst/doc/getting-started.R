## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----testing-mode-------------------------------------------------------------
Sys.setenv(IGFETCHR_TESTING = "true")
library(igfetchr)

## ----auth---------------------------------------------------------------------
auth <- ig_auth(
  username = "demo_user",
  password = "demo_pass",
  api_key = "demo_api_key",
  acc_type = "DEMO",
  acc_number = "ABC123"
)
auth

## ----search-------------------------------------------------------------------
mock_markets <- data.frame(
  epic = c("CS.D.USDCHF.CFD.IP"),
  instrumentName = c("USD/CHF"),
  stringsAsFactors = FALSE
)

markets <- ig_search_markets("USD/CHF", auth = auth, mock_response = mock_markets)
markets

## ----price--------------------------------------------------------------------
mock_price <- data.frame(
  bid = 0.8500,
  offer = 0.8504,
  timestamp = Sys.time(),
  stringsAsFactors = FALSE
)
price <- ig_get_price("CS.D.USDCHF.CFD.IP", auth = auth, mock_response = mock_price)
price

## ----historical---------------------------------------------------------------
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
hist

## ----accounts-----------------------------------------------------------------
mock_accounts <- data.frame(
  accountId = "ACCT123",
  balance = 10000,
  preferred = TRUE,
  stringsAsFactors = FALSE
)
accounts <- ig_get_accounts(auth = auth, mock_response = mock_accounts)
accounts

## ----trade--------------------------------------------------------------------
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

## ----logout-------------------------------------------------------------------
ig_close_session(auth, mock_response = TRUE)
Sys.unsetenv("IGFETCHR_TESTING")

