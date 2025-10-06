library(testthat)

test_that("ig_auth returns mock tokens in testing mode", {
  Sys.setenv(IGFETCHR_TESTING = "true")
  on.exit(Sys.unsetenv("IGFETCHR_TESTING"))
  auth <- ig_auth("user", "pass", "key", acc_type = "DEMO")
  expect_type(auth, "list")
  expect_true(all(c("cst", "security", "base_url") %in% names(auth)))
  # New ig_auth returns api_key and acc_type as well; be tolerant if acc_number is NULL
  expect_true(all(c("api_key", "acc_type") %in% names(auth)))
  expect_true(grepl("demo-api", auth$base_url) || grepl("api.ig.com", auth$base_url))
})

test_that("ig_search_markets returns tibble from mock_response", {
  # Mock response matching the expected API structure
  mock <- list(
    markets = list(
      list(epic = "TEST.EPIC", name = "Test Market")
    )
  )
  res <- ig_search_markets(
    query = "test",
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_true("epic" %in% names(res))
})

test_that("ig_get_markets_by_epic returns tibble from valid mock_response", {
  # Mock response mimicking the actual API response for CS.D.USDCHF.MINI.IP
  mock <- list(
    marketDetails = list(
      list(
        instrument = list(
          epic = "CS.D.USDCHF.MINI.IP",
          expiry = "-",
          name = "USD/CHF Mini",
          forceOpenAllowed = TRUE,
          stopsLimitsAllowed = TRUE,
          lotSize = 1,
          unit = "CONTRACTS",
          type = "CURRENCIES",
          controlledRiskAllowed = TRUE,
          streamingPricesAvailable = TRUE,
          marketId = "USDCHF",
          currencies = list(
            list(
              code = "CHF",
              symbol = "SF",
              baseExchangeRate = 0.0848,
              exchangeRate = 0.6,
              isDefault = FALSE
            )
          ),
          sprintMarketsMinimumExpiryTime = NULL,
          sprintMarketsMaximumExpiryTime = NULL,
          marginDepositBands = list(
            list(min = 0, max = 124, margin = 3.33, currency = "CHF"),
            list(min = 124, max = 310, margin = 3.33, currency = "CHF"),
            list(min = 310, max = 930, margin = 6, currency = "CHF"),
            list(min = 930, max = NULL, margin = 15, currency = "CHF")
          ),
          marginFactor = 3.33,
          marginFactorUnit = "PERCENTAGE",
          slippageFactor = list(unit = "pct", value = 50),
          limitedRiskPremium = list(value = 2.25, unit = "POINTS"),
          openingHours = NULL,
          expiryDetails = NULL,
          rolloverDetails = NULL,
          newsCode = "CHF=",
          chartCode = "USDCHF",
          country = NULL,
          valueOfOnePip = "1.00",
          onePipMeans = "0.0001 CHF/USD",
          contractSize = "10000",
          specialInfo = list(
            "DEFAULT KNOCK OUT LEVEL DISTANCE",
            "MIN KNOCK OUT LEVEL DISTANCE",
            "MAX KNOCK OUT LEVEL DISTANCE"
          )
        ),
        dealingRules = list(
          minStepDistance = list(unit = "POINTS", value = 1),
          minDealSize = list(unit = "POINTS", value = 0.1),
          minControlledRiskStopDistance = list(unit = "PERCENTAGE", value = 2),
          minNormalStopOrLimitDistance = list(unit = "POINTS", value = 4),
          maxStopOrLimitDistance = list(unit = "PERCENTAGE", value = 75),
          controlledRiskSpacing = list(unit = "POINTS", value = 25),
          marketOrderPreference = "AVAILABLE_DEFAULT_OFF",
          trailingStopsPreference = "AVAILABLE"
        ),
        snapshot = list(
          marketStatus = "TRADEABLE",
          netChange = 0.00168,
          percentageChange = 0.21,
          updateTime = "14:24:05",
          delayTime = 0,
          bid = 0.797,
          offer = 0.797,
          high = 0.8,
          low = 0.795,
          binaryOdds = NULL,
          decimalPlacesFactor = 5,
          scalingFactor = 10000,
          controlledRiskExtraSpread = 2
        )
      )
    )
  )
  res <- ig_get_markets_by_epic(
    epics = "CS.D.USDCHF.MINI.IP",
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    detailed = TRUE,
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_true(all(c("instrument", "dealingRules", "snapshot") %in% names(res)))
  expect_equal(length(res$instrument[[1]]), 29)  # Verify instrument has 29 fields
  expect_equal(length(res$dealingRules[[1]]), 8)  # Verify dealingRules has 8 fields
  expect_equal(length(res$snapshot[[1]]), 13)     # Verify snapshot has 13 fields
  expect_equal(res$instrument[[1]]$epic, "CS.D.USDCHF.MINI.IP")
  expect_equal(res$snapshot[[1]]$bid, 0.797)
})


test_that("ig_get_markets_by_epic respects detailed = FALSE", {
  mock <- list(
    marketDetails = list(
      list(
        instrument = list(
          epic = "CS.D.USDCHF.MINI.IP",
          name = "USD/CHF Mini"
        ),
        dealingRules = list(),
        snapshot = list(
          marketStatus = "TRADEABLE",
          bid = 0.797,
          offer = 0.797
        )
      )
    )
  )
  res <- ig_get_markets_by_epic(
    epics = "CS.D.USDCHF.MINI.IP",
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    detailed = FALSE,
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_true(all(c("instrument", "dealingRules", "snapshot") %in% names(res)))
  expect_equal(res$instrument[[1]]$epic, "CS.D.USDCHF.MINI.IP")
})


test_that("ig_get_price handles mock_response and returns tibble", {
  # Mock response matching the expected API structure
  mock <- list(
    snapshot = list(
      bid = 1.23,
      offer = 1.25
    )
  )
  res <- ig_get_price(
    epic = "TEST.EPIC",
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("bid", "offer") %in% names(res)))
})

test_that("ig_get_historical returns tibble from valid mock_response", {
  # Mock response mimicking the API structure for CS.D.USDCHF.MINI.IP
  mock <- list(
    prices = list(
      list(
        snapshotTime = "2025/09/01 00:00:00",
        openPrice = list(bid = 0.800, ask = 0.801),
        closePrice = list(bid = 0.800, ask = 0.801),
        highPrice = list(bid = 0.802, ask = 0.802),
        lowPrice = list(bid = 0.798, ask = 0.799),
        lastTradedVolume = 54435
      ),
      list(
        snapshotTime = "2025/09/02 00:00:00",
        openPrice = list(bid = 0.800, ask = 0.801),
        closePrice = list(bid = 0.804, ask = 0.805),
        highPrice = list(bid = 0.806, ask = 0.806),
        lowPrice = list(bid = 0.800, ask = 0.800),
        lastTradedVolume = 100119
      )
    )
  )
  res <- ig_get_historical(
    epic = "CS.D.USDCHF.MINI.IP",
    from = "2025-09-01",
    to = "2025-09-28",
    resolution = "D",
    page_size = 20,
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)  # Mock contains 2 entries
  expect_true(all(c("snapshotTime", "open_bid", "open_ask", "close_bid", "close_ask",
                    "high_bid", "high_ask", "low_bid", "low_ask", "volume") %in% names(res)))
  expect_equal(res$snapshotTime[1], "2025/09/01 00:00:00")
  expect_equal(res$close_bid[1], 0.800)
  expect_equal(res$volume[2], 100119)
  expect_type(res$snapshotTime, "character")
  expect_type(res$open_bid, "double")
  expect_type(res$volume, "integer")  # Expect integer after function fix
})

test_that("ig_get_historical handles empty prices", {
  mock <- list(prices = list())
  res <- ig_get_historical(
    epic = "CS.D.USDCHF.MINI.IP",
    from = "2025-09-01",
    to = "2025-09-28",
    resolution = "D",
    page_size = 20,
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("snapshotTime", "open_bid", "open_ask", "close_bid", "close_ask",
                    "high_bid", "high_ask", "low_bid", "low_ask", "volume") %in% names(res)))
})

test_that("ig_get_historical handles missing prices", {
  mock <- list(otherField = "invalid")
  res <- ig_get_historical(
    epic = "CS.D.USDCHF.MINI.IP",
    from = "2025-09-01",
    to = "2025-09-28",
    resolution = "D",
    page_size = 20,
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("snapshotTime", "open_bid", "open_ask", "close_bid", "close_ask",
                    "high_bid", "high_ask", "low_bid", "low_ask", "volume") %in% names(res)))
})

test_that("ig_get_historical respects resolution and page_size", {
  mock <- list(
    prices = list(
      list(
        snapshotTime = "2025/09/01 00:00:00",
        openPrice = list(bid = 0.800, ask = 0.801),
        closePrice = list(bid = 0.800, ask = 0.801),
        highPrice = list(bid = 0.802, ask = 0.802),
        lowPrice = list(bid = 0.798, ask = 0.799),
        lastTradedVolume = 54435
      )
    )
  )
  res <- ig_get_historical(
    epic = "CS.D.USDCHF.MINI.IP",
    from = "2025-09-01",
    to = "2025-09-28",
    resolution = "W",
    page_size = 10,
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_true(all(c("snapshotTime", "open_bid", "open_ask", "close_bid", "close_ask",
                    "high_bid", "high_ask", "low_bid", "low_ask", "volume") %in% names(res)))
})



test_that("ig_get_accounts returns tibble from valid mock_response", {
  # Mock response mimicking the API structure for /accounts endpoint
  mock <- list(
    accounts = list(
      list(
        accountId = "Z5AMR1",
        accountName = "CFD",
        accountAlias = NULL,
        status = "ENABLED",
        accountType = "CFD",
        preferred = TRUE,
        currency = "SEK",
        canTransferFrom = TRUE,
        canTransferTo = TRUE,
        balance = list(
          balance = 1000.0,
          deposit = 500.0,
          profitLoss = 200.0,
          available = 800.0
        )
      ),
      list(
        accountId = "Z5AMR2",
        accountName = "Barriers och optioner",
        accountAlias = NULL,
        status = "ENABLED",
        accountType = "CFD",
        preferred = FALSE,
        currency = "SEK",
        canTransferFrom = TRUE,
        canTransferTo = TRUE,
        balance = list(
          balance = 2000.0,
          deposit = 1000.0,
          profitLoss = 300.0,
          available = 1700.0
        )
      )
    )
  )
  res <- ig_get_accounts(
    auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
    mock_response = mock
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)  # Mock contains 2 accounts
  expect_true(all(c("accountId", "accountName", "accountAlias", "status", "accountType",
                    "preferred", "currency", "canTransferFrom", "canTransferTo",
                    "balance", "deposit", "profitLoss", "available") %in% names(res)))
  expect_equal(res$accountId[1], "Z5AMR1")
  expect_equal(res$accountName[2], "Barriers och optioner")
  expect_equal(res$accountAlias[1], NA)
  expect_type(res$accountId, "character")
  expect_type(res$accountName, "character")
  expect_type(res$accountAlias, "logical")
  expect_type(res$status, "character")
  expect_type(res$accountType, "character")
  expect_type(res$preferred, "logical")
  expect_type(res$currency, "character")
  expect_type(res$canTransferFrom, "logical")
  expect_type(res$canTransferTo, "logical")
  expect_type(res$balance, "double")
  expect_type(res$deposit, "double")
  expect_type(res$profitLoss, "double")
  expect_type(res$available, "double")
})

test_that("ig_get_accounts handles empty accounts", {
  mock <- list(accounts = list())
  res <- ig_get_accounts(auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"), mock_response = mock)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("accountId", "accountName", "accountAlias", "status", "accountType",
                    "preferred", "currency", "canTransferFrom", "canTransferTo",
                    "balance", "deposit", "profitLoss", "available") %in% names(res)))
})

test_that("ig_get_accounts handles missing accounts", {
  mock <- list(otherField = "invalid")
  res <- ig_get_accounts(auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"), mock_response = mock)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_true(all(c("accountId", "accountName", "accountAlias", "status", "accountType",
                    "preferred", "currency", "canTransferFrom", "canTransferTo",
                    "balance", "deposit", "profitLoss", "available") %in% names(res)))
})

test_that("ig_logout / ig_close_session returns TRUE in testing mode", {
  Sys.setenv(IGFETCHR_TESTING = "true")
  on.exit(Sys.unsetenv("IGFETCHR_TESTING"))
  res <- ig_logout(list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"))
  # ig_close_session/ig_logout returns invisible(TRUE) in testing mode; when assigned it should be TRUE
  expect_true(isTRUE(res))
})
