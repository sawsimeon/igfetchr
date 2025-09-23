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
  mock <- data.frame(epic = "TEST.EPIC", name = "Test Market", stringsAsFactors = FALSE)
  res <- ig_search_markets("test", auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"), mock_response = mock)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_true("epic" %in% names(res))
})

test_that("ig_get_price handles mock_response and returns tibble", {
  mock <- data.frame(bid = 1.23, offer = 1.25, stringsAsFactors = FALSE)
  res <- ig_get_price("TEST.EPIC", auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"), mock_response = mock)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("bid", "offer") %in% names(res)))
})

test_that("ig_get_historical handles query params and mock_response", {
  mock <- data.frame(date = c("2020-01-01", "2020-01-02"), open = c(1,2), close = c(2,3), stringsAsFactors = FALSE)
  res <- ig_get_historical("TEST.EPIC", from = "2020-01-01", to = "2020-01-02", resolution = "D",
                           auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"),
                           mock_response = mock)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true("date" %in% names(res))
})

test_that("ig_get_accounts returns tibble from mock_response", {
  mock <- data.frame(accountId = "ABC123", balance = 1000, stringsAsFactors = FALSE)
  res <- ig_get_accounts(auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"), mock_response = mock)
  expect_s3_class(res, "tbl_df")
  expect_true("accountId" %in% names(res))
})

test_that("ig_logout / ig_close_session returns TRUE in testing mode", {
  Sys.setenv(IGFETCHR_TESTING = "true")
  on.exit(Sys.unsetenv("IGFETCHR_TESTING"))
  res <- ig_logout(list(base_url = "https://demo-api.ig.com", cst = "x", security = "y", api_key = "k"))
  # ig_close_session/ig_logout returns invisible(TRUE) in testing mode; when assigned it should be TRUE
  expect_true(isTRUE(res))
})
