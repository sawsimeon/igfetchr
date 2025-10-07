library(testthat)

test_that("ig_get_options filters option-like positions from mock_response", {
  # Mock response mimicking the API structure for /positions endpoint
  mock <- list(
    positions = list(
      list(
        position = list(
          contractSize = 100,
          createdDate = "2025/10/01 12:00:00",
          dealId = "D1",
          dealSize = 1,
          direction = "BUY",
          limitLevel = 150,
          openLevel = 145,
          currency = "USD",
          controlledRisk = FALSE,
          stopLevel = 140,
          trailingStep = NULL,
          trailingStopDistance = NULL,
          limitedRiskPremium = NULL
        ),
        market = list(
          instrumentName = "Call A",
          expiry = "DEC-25",
          epic = "OP.CALLA.DEC25.IP",
          instrumentType = "OPTION",
          lotSize = 1,
          high = 150,
          low = 140,
          percentageChange = 2.5,
          netChange = 5,
          bid = 145,
          offer = 146,
          updateTime = "2025/10/01 12:00:00",
          delayTime = 0,
          streamingPricesAvailable = TRUE,
          marketStatus = "TRADEABLE",
          scalingFactor = 1
        )
      ),
      list(
        position = list(
          contractSize = 200,
          createdDate = "2025/10/01 12:00:00",
          dealId = "D2",
          dealSize = 2,
          direction = "SELL",
          limitLevel = 1.2000,
          openLevel = 1.1950,
          currency = "USD",
          controlledRisk = FALSE,
          stopLevel = 1.2050,
          trailingStep = NULL,
          trailingStopDistance = NULL,
          limitedRiskPremium = NULL
        ),
        market = list(
          instrumentName = "Spot EURUSD",
          expiry = "-",
          epic = "CS.D.EURUSD.MINI.IP",
          instrumentType = "SPOT",
          lotSize = 1,
          high = 1.2100,
          low = 1.1900,
          percentageChange = -0.5,
          netChange = -0.0050,
          bid = 1.1950,
          offer = 1.1960,
          updateTime = "2025/10/01 12:00:00",
          delayTime = 0,
          streamingPricesAvailable = TRUE,
          marketStatus = "TRADEABLE",
          scalingFactor = 1
        )
      ),
      list(
        position = list(
          contractSize = 300,
          createdDate = "2025/10/01 12:00:00",
          dealId = "D3",
          dealSize = 3,
          direction = "BUY",
          limitLevel = 200,
          openLevel = 195,
          currency = "USD",
          controlledRisk = FALSE,
          stopLevel = 190,
          trailingStep = NULL,
          trailingStopDistance = NULL,
          limitedRiskPremium = NULL
        ),
        market = list(
          instrumentName = "Put B",
          expiry = "DEC-25",
          epic = "OP.PUTB.DEC25.IP",
          instrumentType = "OPTION",
          lotSize = 1,
          high = 200,
          low = 190,
          percentageChange = 1.5,
          netChange = 3,
          bid = 195,
          offer = 196,
          updateTime = "2025/10/01 12:00:00",
          delayTime = 0,
          streamingPricesAvailable = TRUE,
          marketStatus = "TRADEABLE",
          scalingFactor = 1
        )
      )
    )
  )
  
  res <- ig_get_options(
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  # Should only contain rows where instrumentType indicates OPTION
  expect_true(all(res$instrumentType %in% c("OPTION", "DERIVATIVE", "OPTION_CONTRACT")))
  expect_equal(nrow(res), 2)
  expect_true(all(c("dealId", "instrumentName", "size") %in% names(res)))
  expect_equal(res$dealId, c("D1", "D3"))
  expect_equal(res$instrumentName, c("Call A", "Put B"))
})

test_that("ig_get_options handles empty positions", {
  mock <- list(positions = list())
  res <- ig_get_options(auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"), mock_response = mock)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("dealId", "instrumentName", "size", "instrumentType") %in% names(res)))
})

test_that("ig_get_options handles missing positions", {
  mock <- list(otherField = "invalid")
  res <- ig_get_options(auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"), mock_response = mock)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("dealId", "instrumentName", "size", "instrumentType") %in% names(res)))
})

# Mock ig_get_markets_by_epic with correct instrument structure
mock_ig_get_markets_by_epic <- function(epic, auth, detailed = FALSE) {
  if (epic == "CS.D.USDCHF.MINI.IP") {
    return(list(
      instrument = list(  # Single list, not a list of lists
        epic = "CS.D.USDCHF.MINI.IP",
        expiry = "-",
        currencies = list(list(code = "CHF")),
        marketStatus = "TRADEABLE"
      ),
      snapshot = list(
        bid = 0.795,
        offer = 0.795,
        scalingFactor = 10000
      )
    ))
  }
  stop("Invalid epic")
}
assignInNamespace("ig_get_markets_by_epic", mock_ig_get_markets_by_epic, ns = "igfetchr")


test_that("ig_execute_trade fails with invalid epic", {
  mock_auth <- list(
    base_url = "https://demo-api.ig.com",
    api_key = "mock_api_key",
    cst = "mock_cst",
    security = "mock_security_token",
    acc_number = "mock_account"
  )
  
  expect_error(
    ig_execute_trade(
      epic = "",
      direction = "BUY",
      size = 1.0,
      auth = mock_auth,
      currency_code = "CHF",
      order_type = "MARKET",
      time_in_force = "FILL_OR_KILL"
    ),
    regexp = "is.character\\(epic\\) && nchar\\(epic\\) > 0 is not TRUE"
  )
})

test_that("ig_execute_trade fails with invalid direction", {
  mock_auth <- list(
    base_url = "https://demo-api.ig.com",
    api_key = "mock_api_key",
    cst = "mock_cst",
    security = "mock_security_token",
    acc_number = "mock_account"
  )
  
  expect_error(
    ig_execute_trade(
      epic = "CS.D.USDCHF.MINI.IP",
      direction = "INVALID",
      size = 1.0,
      auth = mock_auth,
      currency_code = "CHF",
      order_type = "MARKET",
      time_in_force = "FILL_OR_KILL"
    ),
    regexp = "is.character\\(direction\\) && direction %in% c\\(\"BUY\", \"SELL\"\\) is not TRUE"
  )
})

test_that("ig_execute_trade fails with invalid size", {
  mock_auth <- list(
    base_url = "https://demo-api.ig.com",
    api_key = "mock_api_key",
    cst = "mock_cst",
    security = "mock_security_token",
    acc_number = "mock_account"
  )
  
  expect_error(
    ig_execute_trade(
      epic = "CS.D.USDCHF.MINI.IP",
      direction = "BUY",
      size = -1,
      auth = mock_auth,
      currency_code = "CHF",
      order_type = "MARKET",
      time_in_force = "FILL_OR_KILL"
    ),
    regexp = "is.numeric\\(size\\) && size > 0 is not TRUE"
  )
})

test_that("ig_execute_trade fails with invalid auth", {
  expect_error(
    ig_execute_trade(
      epic = "CS.D.USDCHF.MINI.IP",
      direction = "BUY",
      size = 1.0,
      auth = NULL,
      currency_code = "CHF",
      order_type = "MARKET",
      time_in_force = "FILL_OR_KILL"
    ),
    regexp = "`auth` must be a list from ig_auth\\(\\)"
  )
})

test_that("ig_execute_trade fails with invalid deal_reference", {
  mock_auth <- list(
    base_url = "https://demo-api.ig.com",
    api_key = "mock_api_key",
    cst = "mock_cst",
    security = "mock_security_token",
    acc_number = "mock_account"
  )
  
  expect_error(
    ig_execute_trade(
      epic = "CS.D.USDCHF.MINI.IP",
      direction = "BUY",
      size = 1.0,
      auth = mock_auth,
      currency_code = "CHF",
      order_type = "MARKET",
      time_in_force = "FILL_OR_KILL",
      deal_reference = "INVALID@REFERENCE"
    ),
    regexp = "is.null\\(deal_reference\\) \\|\\| \\(is.character\\(deal_reference\\) && grepl.* is not TRUE"
  )
})


test_that("ig_close_session works in testing mode and ig_logout returns TRUE", {
  Sys.setenv(IGFETCHR_TESTING = "true")
  on.exit(Sys.unsetenv("IGFETCHR_TESTING"))

  expect_true(isTRUE(ig_close_session(list(base_url = "https://demo-api.ig.com", cst = "x", security = "y"))))
  expect_true(isTRUE(ig_logout(list(base_url = "https://demo-api.ig.com", cst = "x", security = "y"))))
})

