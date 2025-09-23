library(testthat)

test_that("ig_get_options filters option-like positions from mock_response", {
  mock_positions <- data.frame(
    dealId = c("D1", "D2", "D3"),
    instrumentName = c("Call A", "Spot EURUSD", "Put B"),
    instrumentType = c("OPTION", "SPOT", "OPTION"),
    size = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  res <- ig_get_options(auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y"), mock_response = mock_positions)
  expect_s3_class(res, "tbl_df")
  # Should only contain rows where instrumentType indicates OPTION
  expect_true(all(res$instrumentType %in% c("OPTION", "DERIVATIVE", "OPTION_CONTRACT")))
  expect_equal(nrow(res), 2)
  expect_true(all(c("dealId", "instrumentName", "size") %in% names(res)))
})

test_that("ig_execute_trade accepts mock_response and returns tibble with trade confirmation", {
  mock_trade <- data.frame(
    dealId = "TRADE123",
    status = "SUCCESS",
    dealReference = "REF123",
    stringsAsFactors = FALSE
  )

  res <- ig_execute_trade("TEST.EPIC", "BUY", 1.0,
                          auth = list(base_url = "https://demo-api.ig.com", cst = "x", security = "y"),
                          mock_response = mock_trade)
  expect_s3_class(res, "tbl_df")
  expect_equal(as.character(res$dealId[1]), "TRADE123")
  expect_equal(as.character(res$status[1]), "SUCCESS")
  expect_true("dealReference" %in% names(res))
})

test_that("ig_close_session works in testing mode and ig_logout returns TRUE", {
  Sys.setenv(IGFETCHR_TESTING = "true")
  on.exit(Sys.unsetenv("IGFETCHR_TESTING"))

  expect_true(isTRUE(ig_close_session(list(base_url = "https://demo-api.ig.com", cst = "x", security = "y"))))
  expect_true(isTRUE(ig_logout(list(base_url = "https://demo-api.ig.com", cst = "x", security = "y"))))
})
