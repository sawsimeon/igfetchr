#' igfetchr: Lightweight IG Trading REST API wrapper
#'
#' Helper functions to authenticate and fetch market/account data from the IG Trading
#' REST API. All exported functions return tibbles where appropriate. This package is
#' not financial advice. Trading CFDs and spread bets carries high risk of losing money.
#' Key functions: `ig_auth` to authenticate, `ig_execute_trade` to place trades.
#'
#' @docType package
#' @name igfetchr
#' @keywords internal
"_PACKAGE"

#' Authenticate with the IG API
#'
#' Authenticates with the IG API to obtain session tokens (CST and X-SECURITY-TOKEN)
#' for subsequent API requests. Supports environment variables for credentials and
#' optional account type and number.
#'
#' @param username Character. IG account username. Defaults to `IG_SERVICE_USERNAME`.
#' @param password Character. IG account password. Defaults to `IG_SERVICE_PASSWORD`.
#' @param api_key Character. IG API key. Defaults to `IG_SERVICE_API_KEY`.
#' @param acc_type Character. Account type, either "DEMO" or "LIVE". 
#'  Defaults to "DEMO".
#' @param acc_number Character. Optional account number. 
#'  Defaults to `IG_SERVICE_ACC_NUMBER` or NULL.
#'
#' @return A list containing client session token (cst), security token (X-SECURITY-TOKEN), 
#' base URL, API key, account type, and account number (or NULL if not provided).
#'
#' @examples
#' \dontrun{
#' # Using environment variables
#' Sys.setenv(IG_SERVICE_USERNAME = "your_username")
#' Sys.setenv(IG_SERVICE_PASSWORD = "your_password")
#' Sys.setenv(IG_SERVICE_API_KEY = "your_api_key")
#' Sys.setenv(IG_SERVICE_ACC_NUMBER = "ABC123")
#' Sys.setenv(IG_SERVICE_ACC_TYPE = "DEMO")
#' auth <- ig_auth()
#'
#' # Using explicit arguments
#' auth <- ig_auth(
#'   username = "your_username",
#'   password = "your_password",
#'   api_key = "your_api_key",
#'   acc_type = "DEMO",
#'   acc_number = "ABC123"
#' )
#' }
#'
#' @export
ig_auth <- function(username = Sys.getenv("IG_SERVICE_USERNAME"),
                    password = Sys.getenv("IG_SERVICE_PASSWORD"),
                    api_key = Sys.getenv("IG_SERVICE_API_KEY"),
                    acc_type = Sys.getenv("IG_SERVICE_ACC_TYPE", "DEMO"),
                    acc_number = Sys.getenv("IG_SERVICE_ACC_NUMBER")) {
  # Use environment variables if set and non-empty, otherwise use provided arguments
  username <- if (nchar(username) > 0) username else stop("`username` must be provided 
                                                          via IG_SERVICE_USERNAME or function argument.")
  password <- if (nchar(password) > 0) password else stop("`password` must be provided 
                                                          via IG_SERVICE_PASSWORD or function argument.")
  api_key <- if (nchar(api_key) > 0) api_key else stop("`api_key` must be provided 
                                                       via IG_SERVICE_API_KEY or function argument.")
  acc_type <- if (nchar(acc_type) > 0) acc_type else "DEMO"  # Default to DEMO if not set
  acc_number <- if (nchar(acc_number) > 0) acc_number else NULL  # acc_number is optional

  stopifnot(is.character(username), is.character(password), is.character(api_key))
  if (!is.character(acc_type) || length(acc_type) != 1) stop("`acc_type` must be a 
                                                             single character, e.g. 'DEMO' or 'LIVE'")
  acc_type_upper <- toupper(acc_type)
  base_url <- if (identical(acc_type_upper, "DEMO")) "https://demo-api.ig.com/gateway/deal" else "https://api.ig.com/gateway/deal"

  # Support offline/mock mode for tests
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    return(list(cst = "mock_cst", security = "mock_security", base_url = base_url, 
                api_key = api_key, acc_type = acc_type_upper, acc_number = acc_number))
  }

  body_list <- list(identifier = username, password = password)
  if (!is.null(acc_type)) body_list$accountType <- acc_type_upper
  if (!is.null(acc_number)) body_list$accountNumber <- acc_number
  body <- jsonlite::toJSON(body_list, auto_unbox = TRUE)
  resp <- httr::POST(
    url = paste0(base_url, "/session"),
    httr::add_headers(
      "X-IG-API-KEY" = api_key,
      "Content-Type" = "application/json; charset=UTF-8"
    ),
    body = body,
    encode = "raw"
  )

  status <- httr::status_code(resp)
  if (status != 200) {
    msg <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"), error = function(e) "")
    stop("Authentication failed (status ", status, "): ", msg)
  }

  headers <- httr::headers(resp)
  cst <- headers[["CST"]]
  security <- headers[["X-SECURITY-TOKEN"]]
  if (is.null(cst) || is.null(security)) {
    stop("Authentication did not return required headers (CST / X-SECURITY-TOKEN).")
  }

  list(
    cst = cst,
    security = security,
    base_url = base_url,
    api_key = api_key,
    acc_type = acc_type_upper,
    acc_number = acc_number
  )
}

#' Internal function to make HTTP requests to IG API
#'
#' @param path Character. API endpoint path (e.g., "/positions/otc").
#' @param auth List. Authentication details from ig_auth().
#' @param method Character. HTTP method ("GET", "POST", "PUT", "DELETE").
#' @param query List. Query parameters for GET requests. Defaults to list().
#' @param body List. Request body for POST or PUT requests. Defaults to NULL.
#' @param version Character. API version ("1", "2", "3"). Defaults to NULL.
#' @param mock_response List or data frame. 
#'  Optional mock response for testing, bypassing the API call.
#'
#' @return List with API response (status code and body) or tibble if mock_response 
#'  is a data frame.
#'
#' @keywords internal
.ig_request <- function(path, auth, method = c("GET", "POST", "PUT", "DELETE"), query = list(), body = NULL, version = NULL, mock_response = NULL) {
  method <- match.arg(method)
  if (!is.null(mock_response)) {
    if (is.data.frame(mock_response)) {
      return(tibble::as_tibble(mock_response))
    }
    return(mock_response)
  }
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    stop("Network calls disabled during tests. 
         Provide `mock_response` or set IGFETCHR_TESTING to '' to enable network.")
  }
  if (is.null(auth) || !is.list(auth) || is.null(auth$base_url)) {
    stop("`auth` must be a list returned from ig_auth() with a base_url element.")
  }
  if (is.null(auth$api_key) || !is.character(auth$api_key) || nchar(auth$api_key) == 0) {
    stop("`auth` must contain a non-empty api_key.")
  }
  
  url <- paste0(auth$base_url, path)
  
  # Construct headers
  headers_list <- list(
    "X-IG-API-KEY" = auth$api_key,
    "CST" = auth$cst %||% "",
    "X-SECURITY-TOKEN" = auth$security %||% "",
    "Accept" = "application/json"
  )
  if (!is.null(auth$acc_number)) {
    headers_list[["IG-ACCOUNT-ID"]] <- auth$acc_number
  }
  if (!is.null(version)) {
    stopifnot(version %in% c("1", "2", "3"))
    headers_list[["VERSION"]] <- version
  }
  headers <- do.call(httr::add_headers, headers_list)
  
  # Execute request
  resp <- tryCatch({
    switch(
      method,
      GET = httr::GET(url, headers, query = query),
      POST = httr::POST(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "json"),
      PUT = httr::PUT(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "json"),
      DELETE = httr::DELETE(url, headers)
    )
  }, error = function(e) {
    stop("Failed to execute HTTP request: ", e$message)
  })
  
  # Verify response
  if (!inherits(resp, "response")) {
    stop("Invalid response object returned by httr. 
         Check network connection or API availability.")
  }
  
  status <- httr::status_code(resp)
  if (status >= 400) {
    msg <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"), error = function(e) "Unknown error")
    stop("API request failed (status ", status, "): ", msg)
  }
  
  content <- httr::content(resp, "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  return(json)
}

#' Search markets
#'
#' Search markets by text query. Returns a tibble of matching markets from the IG API.
#'
#' @param query Character. Search string for markets (e.g., "USD/CHF").
#' @param auth List. Authentication details from `ig_auth()`, 
#'  including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with market information including epic, instrument name and market status.
#'
#' @examples
#' \dontrun{
#' # Authenticate and search markets
#' auth <- ig_auth(
#'   username = "your_username",
#'   password = "your_password",
#'   api_key = "your_api_key",
#'   acc_type = "DEMO",
#'   acc_number = "ABC123"
#' )
#' markets <- ig_search_markets("USD/CHF", auth)
#' print(markets)
#'
#' # Using mock response
#' mock_response <- data.frame(
#'   epic = "CS.D.USDCHF.MINI.IP",
#'   instrumentName = "USD/CHF Mini",
#'   marketStatus = "OPEN"
#' )
#' markets <- ig_search_markets("USD/CHF", auth, mock_response = mock_response)
#' }
#'
#' @export
ig_search_markets <- function(query, auth, mock_response = NULL) {
  stopifnot(is.character(query))
  path <- paste0("/markets?searchTerm=", utils::URLencode(query, reserved = TRUE))
  
  # Call .ig_request() without api_key parameter
  res <- .ig_request(
    path = path,
    auth = auth,
    method = "GET",
    mock_response = mock_response
  )
  
  # Check if response has markets field
  if (is.null(res$markets)) {
    return(tibble::tibble(epic = character(), name = character()))
  }
  
  # Convert markets list to tibble
  tibble::as_tibble(
    do.call(rbind, lapply(res$markets, function(x) {
      tibble::as_tibble(x)
    }))
  )
}

#' Get market details for one or more epics
#'
#' Fetches detailed market information for the specified market epic(s) from the IG API using the `/markets` endpoint (version 2).
#' Returns instrument details, dealing rules, and snapshot data.
#'
#' @param epics Character vector. One or more market epics (e.g., "CS.D.USDCHF.MINI.IP" or c("CS.D.USDCHF.MINI.IP", "CS.D.EURUSD.MINI.IP")).
#' @param auth List. Authentication details from \code{\link{ig_auth}}.
#' @param detailed Logical. Whether to return detailed info (instrument and dealing rules) or snapshot data only. Defaults to TRUE.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#' @param verbose Logical. Whether to print the raw API response for debugging. Defaults to FALSE.
#'
#' @return A list with market details including instrument (epic, currencies, marginDepositBands), 
#' dealingRules (minStepDistance, minDealSize) and snapshot data (marketStatus, bid, offer, high, low).
#' 
#' @examples
#' \dontrun{
#' # Authenticate with IG API
#' auth <- ig_auth(
#'   username = Sys.getenv("IG_SERVICE_USERNAME"),
#'   password = Sys.getenv("IG_SERVICE_PASSWORD"),
#'   api_key = Sys.getenv("IG_SERVICE_API_KEY"),
#'   acc_type = Sys.getenv("IG_SERVICE_ACC_TYPE"),
#'   acc_number = Sys.getenv("IG_SERVICE_ACC_NUMBER")
#' )
#'
#' # Example 1: Fetch details for a single epic
#' markets <- ig_get_markets_by_epic("CS.D.USDCHF.MINI.IP", auth)
#' print(markets)
#' # Expected output: A tibble with 1 row and 3 columns (instrument, dealingRules, snapshot)
#'
#' # Example 2: Fetch details for multiple epics
#' markets <- ig_get_markets_by_epic(c("CS.D.USDCHF.MINI.IP", "CS.D.EURUSD.MINI.IP"), auth)
#' print(markets)
#' # Expected output: A tibble with 2 rows and 3 columns
#'
#' # Example 3: Fetch snapshot data only (no instrument or dealingRules)
#' markets <- ig_get_markets_by_epic("CS.D.USDCHF.MINI.IP", auth, detailed = FALSE)
#' print(markets)
#' # Expected output: A tibble with 1 row and 1 column (snapshot)
#'
#' # Example 4: Fetch with verbose output for debugging
#' markets <- ig_get_markets_by_epic("CS.D.USDCHF.MINI.IP", auth, verbose = TRUE)
#' print(markets)
#' # Expected output: Prints raw JSON response, followed by a tibble with 1 row and 3 columns
#'
#' # Example 5: Use a mock response for testing
#' mock_response <- list(
#'   marketDetails = list(
#'     list(
#'       instrument = list(
#'         epic = "CS.D.USDCHF.MINI.IP",
#'         name = "USD/CHF Mini",
#'         currencies = list(list(code = "CHF", 
#'         symbol = "SF", 
#'         baseExchangeRate = 0.08467604, 
#'         isDefault = FALSE)),
#'         marginDepositBands = list(
#'           list(min = 0, max = 124, margin = 3.33, currency = "CHF"),
#'           list(min = 124, max = 310, margin = 3.33, currency = "CHF")
#'         ),
#'         marginFactor = 3.33,
#'         marginFactorUnit = "PERCENTAGE"
#'       ),
#'       dealingRules = list(
#'         minStepDistance = list(unit = "POINTS", value = 1.0),
#'         minDealSize = list(unit = "POINTS", value = 0.1)
#'       ),
#'       snapshot = list(
#'         marketStatus = "TRADEABLE",
#'         bid = 0.79715,
#'         offer = 0.79739,
#'         high = 0.79888,
#'         low = 0.79512,
#'         updateTime = "2025/09/29 18:40:51",
#'         binaryOdds = NA,
#'         decimalPlacesFactor = 5,
#'         scalingFactor = 10000
#'       )
#'     )
#'   )
#' )
#' markets <- ig_get_markets_by_epic("CS.D.USDCHF.MINI.IP", auth = NULL, mock_response = mock_response)
#' print(markets)
#' }
#'
#' @export
ig_get_markets_by_epic <- function(epics, auth, detailed = TRUE, mock_response = NULL, verbose = FALSE) {
  stopifnot(is.character(epics), length(epics) > 0, all(nchar(epics) > 0))
  
  # Allow tests to bypass network
  if (!is.null(mock_response)) {
    if (is.list(mock_response) && !is.null(mock_response$marketDetails)) {
      res <- mock_response
    } else {
      res <- list(marketDetails = mock_response)
    }
  } else if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    stop("Network calls disabled during tests. Provide `mock_response` to simulate market fetch.")
  } else {
    if (is.null(auth) || !is.list(auth) || is.null(auth$base_url)) {
      stop("`auth` must be a list returned from ig_auth() with a base_url element.")
    }
    
    # Construct query
    query <- list(
      epics = paste(epics, collapse = ","),
      filter = if (detailed) "ALL" else "SNAPSHOT_ONLY"
    )
    
    # Call .ig_request() and inspect raw response
    res <- tryCatch(
      {
        res <- .ig_request(
          path = "/markets",
          auth = auth,
          method = "GET",
          query = query,
          version = "2"
        )
        if (verbose) {
          message("Raw response from /markets: ", jsonlite::toJSON(res, pretty = TRUE, auto_unbox = TRUE))
        }
        res
      },
      error = function(e) {
        stop("Failed to fetch market details for epics '", paste(epics, collapse = ", "), "': ", e$message)
      }
    )
  }
  
  # Check for marketDetails
  if (!is.list(res) || is.null(res$marketDetails)) {
    warning("No marketDetails in response for epics '", paste(epics, collapse = ", "), "'. Response: ", toString(names(res)))
    return(tibble::tibble(instrument = list(), dealingRules = list(), snapshot = list()))
  }
  
  # Check if marketDetails is empty
  if (length(res$marketDetails) == 0) {
    warning("Empty marketDetails list for epics '", paste(epics, collapse = ", "), "'.")
    return(tibble::tibble(instrument = list(), dealingRules = list(), snapshot = list()))
  }
  
  # Ensure all entries have expected fields
  valid_details <- sapply(res$marketDetails, function(x) {
    is.list(x) && all(c("instrument", "dealingRules", "snapshot") %in% names(x))
  })
  if (!all(valid_details)) {
    warning("Some market details missing expected fields (instrument, dealingRules, snapshot) for epics '",
            paste(epics, collapse = ", "), "'. Response: ", toString(names(res$marketDetails)))
    return(tibble::tibble(instrument = list(), dealingRules = list(), snapshot = list()))
  }
  
  # Convert to tibble with explicit columns
  tibble::tibble(
    instrument = lapply(res$marketDetails, function(x) x$instrument),
    dealingRules = lapply(res$marketDetails, function(x) x$dealingRules),
    snapshot = lapply(res$marketDetails, function(x) x$snapshot)
  )
}

#' Get current price for a market
#'
#' Fetches current price(s) for the given market epic from the IG API using the `/markets/\{epic\}` endpoint.
#'
#' @param epic Character. Market epic (e.g., "CS.D.USDCHF.CFD.IP").
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with price information including makret status, bid, offer, high, low, and update time.
#' 
#' @examples
#' \dontrun{
#' # Authenticate and get price
#' auth <- ig_auth(
#'   username = "your_username",
#'   password = "your_password",
#'   api_key = "your_api_key",
#'   acc_type = "DEMO",
#'   acc_number = "ABC123"
#' )
#' price <- ig_get_price("CS.D.USDCHF.CFD.IP", auth)
#' print(price)
#'
#' # Using mock response for testing
#' mock_response <- list(
#'   snapshot = data.frame(
#'     marketStatus = "TRADEABLE",
#'     bid = 0.798,
#'     offer = 0.798,
#'     high = 0.801,
#'     low = 0.797,
#'     updateTime = "2025/09/26 21:58:57",
#'     binaryOdds = NA
#'   )
#' )
#' price <- ig_get_price("CS.D.USDCHF.CFD.IP", auth, mock_response = mock_response)
#' }
#'
#' @export
ig_get_price <- function(epic, auth, mock_response = NULL) {
  stopifnot(is.character(epic), nchar(epic) > 0)
  
  # Construct path for current prices
  path <- paste0("/markets/", utils::URLencode(epic, reserved = TRUE))
  
  # Call .ig_request()
  res <- tryCatch(
    {
      .ig_request(
        path = path,
        auth = auth,
        method = "GET",
        mock_response = mock_response
      )
    },
    error = function(e) {
      stop("Failed to fetch price for epic '", epic, "': ", e$message)
    }
  )
  
  # Check if response has snapshot field
  if (is.null(res$snapshot)) {
    return(tibble::tibble(bid = numeric(), offer = numeric()))
  }
  
  # Return as tibble
  tibble::as_tibble(purrr::map(res$snapshot, ~ if (is.null(.x)) NA else .x))
}

#' Get historical prices for a market
#'
#' Fetches historical prices for a market epic between specified dates at a given resolution from the IG API.
#' Uses the /prices/\{epic\}/\{resolution\}/\{startDate\}/\{endDate\} endpoint (version 2) with a fallback to version 3.
#'
#' @param epic Character. Market epic (e.g., "CS.D.USDCHF.MINI.IP").
#' @param from Character or Date. Start date (e.g., "2025-09-01" or "2025-09-01 00:00:00"). Required.
#' @param to Character or Date. End date (e.g., "2025-09-28" or "2025-09-28 23:59:59"). Required.
#' @param resolution Character. Resolution code (e.g., "D", "1MIN", "HOUR"). Defaults to "D".
#' @param page_size Integer. Number of data points per page (v3 only). Defaults to 20.
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#' @param wait Numeric. Seconds to wait between paginated API calls (v3 only). Defaults to 1.
#'
#' @return A tibble with historical OHLC data including snapshot time, bid, as, open, close, 
#' high, low prices and last traded volume.
#' 
#' @examples
#' \dontrun{
#' # Authenticate and get historical prices
#' auth <- ig_auth(
#'   username = "your_username",
#'   password = "your_password",
#'   api_key = "your_api_key",
#'   acc_type = "DEMO",
#'   acc_number = "ABC123"
#' )
#' hist <- ig_get_historical(
#'   "CS.D.USDCHF.MINI.IP",
#'   from = "2025-09-01",
#'   to = "2025-09-28",
#'   resolution = "D",
#'   page_size = 20,
#'   auth
#' )
#' print(hist)
#'
#' # Using time
#' hist <- ig_get_historical(
#'   "CS.D.USDCHF.MINI.IP",
#'   from = "2025-09-01 00:00:00",
#'   to = "2025-09-28 23:59:59",
#'   resolution = "D",
#'   page_size = 20,
#'   auth
#' )
#'
#' # Using mock response
#' mock_response <- list(
#'   prices = data.frame(
#'     snapshotTime = "2025/09/01 00:00:00",
#'     openPrice.bid = 0.970,
#'     openPrice.ask = 0.971,
#'     openPrice.lastTraded = NA,
#'     highPrice.bid = 0.975,
#'     highPrice.ask = 0.976,
#'     highPrice.lastTraded = NA,
#'     lowPrice.bid = 0.965,
#'     lowPrice.ask = 0.966,
#'     lowPrice.lastTraded = NA,
#'     closePrice.bid = 0.971,
#'     closePrice.ask = 0.972,
#'     closePrice.lastTraded = NA,
#'     lastTradedVolume = 50000
#'   ),
#'   metadata = list(
#'     allowance = list(remainingAllowance = 10000),
#'     pageData = list(pageNumber = 1, totalPages = 1)
#'   )
#' )
#' hist <- ig_get_historical(
#'   "CS.D.USDCHF.MINI.IP",
#'   from = "2025-09-01",
#'   to = "2025-09-28",
#'   resolution = "D",
#'   page_size = 20,
#'   auth,
#'   mock_response = mock_response
#' )
#' }
#'
#' @export
ig_get_historical <- function(epic, from, to, resolution = "D", page_size = 20, auth, mock_response = NULL, wait = 1) {
  stopifnot(
    is.character(epic), nchar(epic) > 0,
    is.character(from) || inherits(from, "Date"),
    is.character(to) || inherits(to, "Date"),
    is.character(resolution),
    is.numeric(page_size), page_size > 0,
    is.numeric(wait), wait >= 0
  )
  
  # Map resolution codes
  resolution_map <- c(
    "1s" = "SECOND",
    "1Min" = "MINUTE", "M" = "MINUTE",
    "2Min" = "MINUTE_2",
    "3Min" = "MINUTE_3",
    "5Min" = "MINUTE_5",
    "10Min" = "MINUTE_10",
    "15Min" = "MINUTE_15",
    "30Min" = "MINUTE_30",
    "1h" = "HOUR", "H" = "HOUR",
    "2h" = "HOUR_2",
    "3h" = "HOUR_3",
    "4h" = "HOUR_4",
    "D" = "DAY",
    "W" = "WEEK",
    "ME" = "MONTH"
  )
  if (!resolution %in% names(resolution_map)) {
    message("Invalid resolution '", resolution, "', returning as-is. Supported: ", paste(names(resolution_map), collapse=", "))
    resolution <- resolution
  } else {
    resolution <- resolution_map[resolution]
  }
  
  # Parse and validate dates
  from_date <- if (inherits(from, "Date")) from else as.POSIXct(from, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ"))
  to_date <- if (inherits(to, "Date")) to else as.POSIXct(to, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ"))
  if (is.na(from_date) || is.na(to_date)) {
    stop("Invalid date format for 'from' or 'to'. Use YYYY-MM-DD, YYYY-MM-DD HH:MM:SS, or YYYY-MM-DDTHH:MM:SS.")
  }
  if (to_date < from_date) {
    stop("'to' date must be after 'from' date.")
  }
  
  # Check date range (max 90 days for DEMO accounts)
  if (as.numeric(to_date - from_date) > 90) {
    message("Date range exceeds 90 days, which may not be supported in DEMO accounts. Consider a shorter range.")
  }
  # Warn if dates are in the future
  current_date <- Sys.time()
  if (from_date > current_date || to_date > current_date) {
    message("Dates are in the future, which may not be supported in DEMO accounts. Current date: ", format(current_date, "%Y-%m-%d %H:%M:%S"))
  }
  
  # Convert dates
  conv_datetime <- function(dt, version = "2") {
    dt_parsed <- tryCatch({
      if (inherits(dt, "POSIXt")) {
        dt
      } else if (inherits(dt, "Date")) {
        as.POSIXct(dt)
      } else {
        as.POSIXct(dt, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ"))
      }
    }, error = function(e) {
      message("Date parsing failed, returning original: ", dt)
      return(dt)
    })
    if (inherits(dt_parsed, "character")) return(dt)
    fmt <- if (version == "3") "%Y-%m-%dT%H:%M:%S" else if (version == "1") "%Y:%m:%d-%H:%M:%S" else "%Y-%m-%d %H:%M:%S"
    format(dt_parsed, fmt)
  }
  
  # Format dates for v2 and v3
  from_v2 <- conv_datetime(from_date, version = "2")
  to_v2 <- conv_datetime(to_date, version = "2")
  from_v3 <- conv_datetime(from_date, version = "3")
  to_v3 <- conv_datetime(to_date, version = "3")
  
  
  path_v2 <- paste0("/prices/", utils::URLencode(epic, reserved = TRUE), "/", resolution, "/", utils::URLencode(from_v2, reserved = TRUE), "/", utils::URLencode(to_v2, reserved = TRUE))
  res <- tryCatch(
    {
      .ig_request(
        path = path_v2,
        auth = auth,
        method = "GET",
        query = list(),
        version = "2",
        mock_response = mock_response
      )
    },
    error = function(e) {
      message("v2 API request failed: ", e$message, "\nRequested URL: ", path_v2)
      # Fallback to v3 endpoint
      path_v3 <- paste0("/prices/", utils::URLencode(epic, reserved = TRUE))
      query_v3 <- list(
        resolution = resolution,
        from = from_v3,
        to = to_v3,
        pageSize = page_size,
        pageNumber = 1
      )
      message("Trying fallback v3 endpoint: ", path_v3, " with query: ", paste(names(query_v3), query_v3, sep="=", collapse="&"))
      prices <- list()
      page_number <- 1
      more_results <- TRUE
      
      while (more_results) {
        query_v3$pageNumber <- page_number
        res_v3 <- tryCatch(
          {
            .ig_request(
              path = path_v3,
              auth = auth,
              method = "GET",
              query = query_v3,
              version = "3",
              mock_response = if (page_number == 1) mock_response else NULL
            )
          },
          error = function(e2) {
            message("v3 API request failed: ", e2$message, "\nRequested URL: ", path_v3, " with query: ", paste(names(query_v3), query_v3, sep="=", collapse="&"))
            stop("Both v2 and v3 API requests failed for epic '", epic, "': ", e$message, " (v2), ", e2$message, " (v3). Check epic validity, date format, or contact IG support at labs.ig.com.")
          }
        )
        
        prices <- append(prices, list(res_v3))
        page_data <- if (is.list(res_v3) && !is.data.frame(res_v3) && !is.null(res_v3$pageData)) res_v3$pageData else list(pageNumber = 1, totalPages = 1)
        if (is.null(page_data) || page_data$totalPages == 0 || page_data$pageNumber == page_data$totalPages) {
          more_results <- FALSE
        } else {
          page_number <- page_number + 1
          Sys.sleep(wait)
        }
      }
      
      if (length(prices) == 0) {
        message("No prices returned from v3 API for epic '", epic, "'. Verify epic and date range with IG support at labs.ig.com.")
        return(tibble::tibble())
      }
      result <- tibble::as_tibble(do.call(rbind, lapply(prices, as.data.frame)))
      if (is.list(res_v3) && !is.data.frame(res_v3) && !is.null(res_v3$metadata)) {
        attr(result, "metadata") <- res_v3$metadata
      }
      return(result)
    }
  )
  
  # Process response
  if (is.list(res) && !is.null(res$prices)) {
    result <- tibble::tibble(
      snapshotTime = sapply(res$prices, `[[`, "snapshotTime"),
      open_bid      = sapply(res$prices, function(x) x$openPrice$bid),
      open_ask      = sapply(res$prices, function(x) x$openPrice$ask),
      close_bid     = sapply(res$prices, function(x) x$closePrice$bid),
      close_ask     = sapply(res$prices, function(x) x$closePrice$ask),
      high_bid      = sapply(res$prices, function(x) x$highPrice$bid),
      high_ask      = sapply(res$prices, function(x) x$highPrice$ask),
      low_bid       = sapply(res$prices, function(x) x$lowPrice$bid),
      low_ask       = sapply(res$prices, function(x) x$lowPrice$ask),
      volume        = as.integer(sapply(res$prices, `[[`, "lastTradedVolume"))
    )
    if (!is.null(res$metadata)) attr(result, "metadata") <- res$metadata
    return(result)
  }
  
  message("No prices returned from API for epic '", epic, "'. Verify epic and date range with IG support at labs.ig.com.")
  return(tibble::tibble(
    snapshotTime = character(),
    open_bid = numeric(),
    open_ask = numeric(),
    close_bid = numeric(),
    close_ask = numeric(),
    high_bid = numeric(),
    high_ask = numeric(),
    low_bid = numeric(),
    low_ask = numeric(),
    volume = integer()
  ))
  
}

#' Retrieve IG account details
#'
#' Fetches details of IG accounts associated with the authenticated session.
#' Returns a tibble containing account information such as account ID, name, balance, and status.
#'
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with account information including account ID, name, balance and currency.
#' 
#' @examples
#' \dontrun{
#' # Authenticate and get accounts
#' auth <- ig_auth(
#'   username = "your_username",
#'   password = "your_password",
#'   api_key = "your_api_key",
#'   acc_type = "DEMO",
#'   acc_number = "ABC123"
#' )
#' accounts <- ig_get_accounts(auth)
#' print(accounts)
#'
#' # Using mock response for testing
#' mock_response <- data.frame(
#'   accountId = "ABC123",
#'   accountName = "Demo Account",
#'   balance.balance = 10000,
#'   currency = "SEK"
#' )
#' accounts <- ig_get_accounts(auth, mock_response = mock_response)
#' }
#'
#' @export
ig_get_accounts <- function(auth, mock_response = NULL) {
  path <- "/accounts"
  
  # Call .ig_request() without api_key parameter
  res <- tryCatch(
    {
      .ig_request(
        path = path,
        auth = auth,
        method = "GET",
        mock_response = mock_response
      )
    },
    error = function(e) {
      stop("Failed to fetch accounts: ", e$message)
    }
  )
  
  # Check for accounts
  if (!is.list(res) || is.null(res$accounts) || length(res$accounts) == 0) {
    message("No accounts returned from API. Verify authentication with IG support at labs.ig.com.")
    return(tibble::tibble(
      accountId = character(),
      accountName = character(),
      accountAlias = logical(),
      status = character(),
      accountType = character(),
      preferred = logical(),
      currency = character(),
      canTransferFrom = logical(),
      canTransferTo = logical(),
      balance = numeric(),
      deposit = numeric(),
      profitLoss = numeric(),
      available = numeric()
    ))
  }
  
  # Return as tibble
  tibble::as_tibble(
    do.call(rbind, lapply(res$accounts, function(x) {
      data.frame(
        accountId        = x$accountId,
        accountName      = x$accountName,
        accountAlias     = ifelse(is.null(x$accountAlias), NA, x$accountAlias),
        status           = x$status,
        accountType      = x$accountType,
        preferred        = x$preferred,
        currency         = x$currency,
        canTransferFrom  = x$canTransferFrom,
        canTransferTo    = x$canTransferTo,
        balance          = x$balance$balance,
        deposit          = x$balance$deposit,
        profitLoss       = x$balance$profitLoss,
        available        = x$balance$available,
        stringsAsFactors = FALSE
      )
    }))
  )
}

#' Get options/derivatives positions
#'
#' Retrieves positions filtered for options/derivatives from the IG API. Returns a tibble with position details.
#'
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with options/derivative position details including deal ID, size, direction and instrument type.
#' 
#' @examples
#' \dontrun{
#' # Authenticate and get options positions
#' auth <- ig_auth(
#'   username = "your_username",
#'   password = "your_password",
#'   api_key = "your_api_key",
#'   acc_type = "DEMO",
#'   acc_number = "ABC123"
#' )
#' options <- ig_get_options(auth)
#' print(options)
#'
#' # Using mock response for testing
#' mock_response <- data.frame(
#'   dealId = "DIXXXX",
#'   size = 1.5,
#'   direction = "BUY",
#'   instrumentType = "OPTION"
#' )
#' options <- ig_get_options(auth, mock_response = mock_response)
#' }
#'
#' @export
ig_get_options <- function(auth, mock_response = NULL) {
  # Using the /positions endpoint; filter for option-like instruments
  path <- "/positions"
  
  # Call .ig_request
  res <- tryCatch(
    {
      .ig_request(
        path = path,
        auth = auth,
        method = "GET",
        mock_response = mock_response
      )
    },
    error = function(e) {
      stop("Failed to fetch positions: ", e$message)
    }
  )
  
  # Check for positions
  if (!is.list(res) || is.null(res$positions) || length(res$positions) == 0) {
    message("No positions returned from API. Verify authentication with IG support at labs.ig.com.")
    return(tibble::tibble(
      contractSize = numeric(),
      createdDate = character(),
      dealId = character(),
      size = numeric(),
      direction = character(),
      limitLevel = numeric(),
      openLevel = numeric(),
      currency = character(),
      controlledRisk = logical(),
      stopLevel = numeric(),
      trailingStep = numeric(),
      trailingStopDistance = numeric(),
      limitedRiskPremium = numeric(),
      instrumentName = character(),
      expiry = character(),
      epic = character(),
      instrumentType = character(),
      lotSize = numeric(),
      high = numeric(),
      low = numeric(),
      percentageChange = numeric(),
      netChange = numeric(),
      bid = numeric(),
      offer = numeric(),
      updateTime = character(),
      delayTime = numeric(),
      streamingPricesAvailable = logical(),
      marketStatus = character(),
      scalingFactor = numeric()
    ))
  }
  
  # Convert to tibble
  result <- tibble::as_tibble(
    do.call(rbind, lapply(res$positions, function(x) {
      data.frame(
        # Position fields
        contractSize         = x$position$contractSize,
        createdDate          = x$position$createdDate,
        dealId               = x$position$dealId,
        size                 = x$position$dealSize,  # Renamed from dealSize to size
        direction            = x$position$direction,
        limitLevel           = x$position$limitLevel,
        openLevel            = x$position$openLevel,
        currency             = x$position$currency,
        controlledRisk       = x$position$controlledRisk,
        stopLevel            = x$position$stopLevel,
        trailingStep         = ifelse(is.null(x$position$trailingStep), NA, x$position$trailingStep),
        trailingStopDistance = ifelse(is.null(x$position$trailingStopDistance), NA, x$position$trailingStopDistance),
        limitedRiskPremium   = ifelse(is.null(x$position$limitedRiskPremium), NA, x$position$limitedRiskPremium),
        
        # Market fields
        instrumentName       = x$market$instrumentName,
        expiry               = x$market$expiry,
        epic                 = x$market$epic,
        instrumentType       = x$market$instrumentType,
        lotSize              = x$market$lotSize,
        high                 = x$market$high,
        low                  = x$market$low,
        percentageChange     = x$market$percentageChange,
        netChange            = x$market$netChange,
        bid                  = x$market$bid,
        offer                = x$market$offer,
        updateTime           = x$market$updateTime,
        delayTime            = x$market$delayTime,
        streamingPricesAvailable = x$market$streamingPricesAvailable,
        marketStatus         = x$market$marketStatus,
        scalingFactor        = x$market$scalingFactor,
        
        stringsAsFactors = FALSE
      )
    }))
  )
  
  # Filter for option-like instruments
  result[result$instrumentType %in% c("OPTION", "DERIVATIVE", "OPTION_CONTRACT"), ]
}

#' Execute a trade (place OTC position)
#'
#' Places a market trade using the IG API.
#' If stops/limits fail, falls back to placing the trade without them and adding via PUT.
#'
#' @param epic Character. Market epic (e.g., "CS.D.USDCHF.MINI.IP").
#' @param direction Character. "BUY" or "SELL".
#' @param size Numeric. Trade size (units).
#' @param auth List. Authentication details from \code{\link{ig_auth}}.
#' @param currency_code Character. Currency code (e.g., "CHF"). Defaults to NULL.
#' @param expiry Character. Expiry date (e.g., "-"). Defaults to NULL.
#' @param guaranteed_stop Logical. Use guaranteed stop. Defaults to FALSE.
#' @param level Numeric. Price level for LIMIT orders. Defaults to NULL.
#' @param time_in_force Character. "EXECUTE_AND_ELIMINATE" or "FILL_OR_KILL". Defaults to "FILL_OR_KILL".
#' @param order_type Character. "MARKET" or "LIMIT". Defaults to "MARKET".
#' @param limit_distance Numeric. Limit distance in points. Defaults to NULL.
#' @param limit_level Numeric. Limit price. Defaults to NULL.
#' @param stop_distance Numeric. Stop distance in points. Defaults to NULL.
#' @param stop_level Numeric. Stop price. Defaults to NULL.
#' @param deal_reference Character. Custom deal reference. Defaults to NULL.
#' @param force_open Logical. Force new position. Defaults to TRUE if stops/limits specified.
#' @param mock_response List or data frame. Mock response for testing.
#'
#' @return A tibble with trade confirmation details including deal ID and deal reference.
#' 
#' @examples
#' \dontrun{
#' auth <- ig_auth(
#' username = "your_username",
#' password = "your_password",
#' api_key = "your_api_key",
#' acc_type = "DEMO",
#' acc_number = "ABC123")
#' res <- ig_execute_trade(
#'   epic = "CS.D.USDCHF.MINI.IP",
#'   direction = "BUY",
#'   size = 1.0,
#'   auth = auth,
#'   currency_code = "CHF",
#'   order_type = "MARKET",
#'   time_in_force = "FILL_OR_KILL",
#'   limit_distance = 2000,
#'   stop_distance = 2000,
#'   guaranteed_stop = FALSE,
#'   force_open = TRUE
#' )
#' print(res)
#' }
#'
#' @export
ig_execute_trade <- function(epic, direction, size, auth, currency_code = NULL, expiry = NULL, guaranteed_stop = FALSE, 
                             level = NULL, time_in_force = "FILL_OR_KILL", order_type = "MARKET", 
                             limit_distance = NULL, limit_level = NULL, stop_distance = NULL, 
                             stop_level = NULL, deal_reference = NULL, force_open = NULL, 
                             mock_response = NULL) {
  
  # Input validation
  stopifnot(
    is.character(epic) && nchar(epic) > 0,
    is.character(direction) && direction %in% c("BUY", "SELL"),
    is.numeric(size) && size > 0,
    is.logical(guaranteed_stop),
    is.character(time_in_force) && time_in_force %in% c("EXECUTE_AND_ELIMINATE", "FILL_OR_KILL"),
    is.character(order_type) && order_type %in% c("MARKET", "LIMIT"),
    is.null(force_open) || is.logical(force_open),
    is.null(currency_code) || is.character(currency_code),
    is.null(expiry) || is.character(expiry),
    is.null(level) || is.numeric(level),
    is.null(limit_distance) || (is.numeric(limit_distance) && limit_distance > 0),
    is.null(limit_level) || is.numeric(limit_level),
    is.null(stop_distance) || (is.numeric(stop_distance) && stop_distance > 0),
    is.null(stop_level) || is.numeric(stop_level),
    is.null(deal_reference) || (is.character(deal_reference) && grepl("^[A-Za-z0-9_-]{1,30}$", deal_reference))
  )
  
  # Set force_open
  force_open <- if (is.null(force_open)) {
    !is.null(limit_distance) || !is.null(stop_distance) || !is.null(limit_level) || !is.null(stop_level)
  } else {
    force_open
  }
  
  # Handle mock response
  if (!is.null(mock_response)) {
    mock_response <- lapply(mock_response, function(x) if (is.null(x)) NA else x)
    return(tibble::as_tibble(mock_response))
  }
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    stop("Network calls disabled during tests. Provide `mock_response`.")
  }
  if (is.null(auth) || !is.list(auth) || is.null(auth$base_url) || is.null(auth$api_key) || is.null(auth$cst) || is.null(auth$security)) {
    stop("`auth` must be a list from ig_auth() with base_url, api_key, cst, and security.")
  }
  
  # Fetch market data
  market <- tryCatch(ig_get_markets_by_epic(epic, auth, detailed = TRUE), error = function(e) NULL)
  
  # Initialize defaults
  currency_code <- currency_code %||% (market$instrument[[1]]$currencies[[1]]$code %||% "CHF")
  expiry <- expiry %||% (market$instrument[[1]]$expiry %||% "-")
  scaling_factor <- market$snapshot$scalingFactor %||% 10000
  current_price <- if (direction == "BUY") market$snapshot$bid %||% 0.795 else market$snapshot$offer %||% 0.795
  
  # Convert distances to levels for POST
  pip_value <- 0.0001 / scaling_factor
  if (!is.null(limit_distance) && is.null(limit_level)) {
    limit_level <- if (direction == "BUY") current_price + (limit_distance * pip_value * scaling_factor) else current_price - (limit_distance * pip_value * scaling_factor)
  }
  if (!is.null(stop_distance) && is.null(stop_level)) {
    stop_level <- if (direction == "BUY") current_price - (stop_distance * pip_value * scaling_factor) else current_price + (stop_distance * pip_value * scaling_factor)
  }
  
  # Construct request body with levels only
  body <- list(
    epic = epic,
    direction = direction,
    size = size,
    currencyCode = currency_code,
    expiry = expiry,
    orderType = order_type,
    guaranteedStop = tolower(guaranteed_stop),
    forceOpen = tolower(force_open),
    timeInForce = time_in_force
  )
  if (!is.null(level)) body$level <- level
  if (!is.null(limit_level)) body$limitLevel <- limit_level
  if (!is.null(stop_level)) body$stopLevel <- stop_level
  if (!is.null(deal_reference)) body$dealReference <- deal_reference
  
  # Try trade with stops/limits, fall back if it fails
  res <- tryCatch(
    {
      .ig_request(
        path = "/positions/otc",
        auth = auth,
        method = "POST",
        body = body,
        version = "2"
      )
    },
    error = function(e) {
      if (grepl("validation.mutual-exclusive-set-value.request", e$message) && 
          (!is.null(limit_level) || !is.null(stop_level))) {
        message("Falling back to trade without stops/limits due to: ", e$message)
        body_fallback <- list(
          epic = epic,
          direction = direction,
          size = size,
          currencyCode = currency_code,
          expiry = expiry,
          orderType = order_type,
          guaranteedStop = "false",
          forceOpen = tolower(force_open),
          timeInForce = time_in_force
        )
        if (!is.null(deal_reference)) body_fallback$dealReference <- deal_reference
        
        res_fallback <- tryCatch(
          .ig_request(
            path = "/positions/otc",
            auth = auth,
            method = "POST",
            body = body_fallback,
            version = "2"
          ),
          error = function(e2) stop("Fallback trade failed: ", e2$message)
        )
        
        deal_ref <- res_fallback$dealReference
        if (!is.character(deal_ref) || nchar(deal_ref) > 30 || nchar(deal_ref) == 0 || !grepl("^[A-Za-z0-9_-]{1,30}$", deal_ref)) {
          message("Invalid dealReference '", deal_ref, "'. Returning trade response.")
          res_fallback <- lapply(res_fallback, function(x) if (is.null(x)) NA else x)
          return(tibble::as_tibble(res_fallback))
        }
        
        confirm_path <- paste0("/confirms/", deal_ref)
        confirm_res <- tryCatch(
          .ig_request(
            path = confirm_path,
            auth = auth,
            method = "GET",
            version = "1"
          ),
          error = function(e3) {
            message("Failed to fetch deal confirmation: ", e3$message)
            res_fallback <- lapply(res_fallback, function(x) if (is.null(x)) NA else x)
            return(tibble::as_tibble(res_fallback))
          }
        )
        
        if (!is.null(limit_level) || !is.null(stop_level)) {
          deal_id <- confirm_res$dealId
          if (!is.character(deal_id) || nchar(deal_id) == 0) {
            message("Invalid dealId '", deal_id, "'. Skipping stop/limit addition.")
            confirm_res <- lapply(confirm_res, function(x) if (is.null(x)) NA else x)
            return(tibble::as_tibble(confirm_res))
          }
          
          Sys.sleep(10) # Increased delay for position registration
          edit_body <- list(
            limitLevel = limit_level %||% (if (direction == "BUY") current_price + (limit_distance * pip_value * scaling_factor) else current_price - (limit_distance * pip_value * scaling_factor)),
            stopLevel = stop_level %||% (if (direction == "BUY") current_price - (stop_distance * pip_value * scaling_factor) else current_price + (stop_distance * pip_value * scaling_factor)),
            guaranteedStop = tolower(guaranteed_stop)
          )
          message("Attempting PUT with dealId: ", deal_id, ", body: ", jsonlite::toJSON(edit_body))
          edit_res <- tryCatch(
            .ig_request(
              path = paste0("/positions/", deal_id, "/otc"),
              auth = auth,
              method = "PUT",
              body = edit_body,
              version = "2"
            ),
            error = function(e4) {
              message("First PUT attempt failed for dealId '", deal_id, "': ", e4$message)
              Sys.sleep(5) # Retry after additional delay
              tryCatch(
                .ig_request(
                  path = paste0("/positions/", deal_id, "/otc"),
                  auth = auth,
                  method = "PUT",
                  body = edit_body,
                  version = "2"
                ),
                error = function(e5) {
                  message("Retry PUT failed for dealId '", deal_id, "': ", e5$message)
                  confirm_res <- lapply(confirm_res, function(x) if (is.null(x)) NA else x)
                  return(tibble::as_tibble(confirm_res))
                }
              )
            }
          )
          confirm_res$editResponse <- edit_res
        }
        
        confirm_res <- lapply(confirm_res, function(x) if (is.null(x)) NA else x)
        return(tibble::as_tibble(confirm_res))
      }
      stop("Failed to execute trade: ", e$message)
    }
  )
  
  # Fetch deal confirmation
  deal_ref <- res$dealReference
  if (is.character(deal_ref) && nchar(deal_ref) <= 30 && nchar(deal_ref) > 0 && grepl("^[A-Za-z0-9_-]{1,30}$", deal_ref)) {
    confirm_path <- paste0("/confirms/", deal_ref)
    confirm_res <- tryCatch(
      .ig_request(
        path = confirm_path,
        auth = auth,
        method = "GET",
        version = "1"
      ),
      error = function(e) {
        message("Failed to fetch deal confirmation: ", e$message)
        res <- lapply(res, function(x) if (is.null(x)) NA else x)
        return(tibble::as_tibble(res))
      }
    )
    confirm_res <- lapply(confirm_res, function(x) if (is.null(x)) NA else x)
    return(tibble::as_tibble(confirm_res))
  }
  
  res <- lapply(res, function(x) if (is.null(x)) NA else x)
  return(tibble::as_tibble(res))
}


#' Close session
#'
#' Closes the authenticated IG API session.
#'
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, 
#' `base_url`, `api_key`, and `acc_number`.
#' @param mock_response Logical. Optional mock response for testing (returns `TRUE` if 
#'  provided). Defaults to `NULL`.
#'
#' @return Logical `TRUE` if the session was closed successfully.
#'
#' @examples
#' \dontrun{
#' # Authenticate and close session
#' auth <- ig_auth(
#'   username = "your_username",
#'   password = "your_password",
#'   api_key = "your_api_key",
#'   acc_type = "DEMO",
#'   acc_number = "ABC123"
#' )
#' ig_close_session(auth)
#'
#' # Using mock response for testing
#' ig_close_session(auth, mock_response = TRUE)
#' }
#'
#' @export
ig_close_session <- function(auth, mock_response = NULL) {
  if (!is.null(mock_response)) {
    return(invisible(TRUE))
  }
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    return(invisible(TRUE))
  }
  if (is.null(auth) || !is.list(auth) || is.null(auth$base_url)) {
    stop("`auth` must be a list returned from ig_auth() with a base_url element.")
  }
  
  path <- "/session"
  tryCatch({
    .ig_request(
      path = path,
      auth = auth,
      method = "DELETE",
      mock_response = mock_response
    )
  }, error = function(e) {
    # Suppress parse errors silently
    invisible(NULL)
  })
  
  invisible(TRUE)
}

#' Close session (alias)
#'
#' Alias for `ig_close_session()`.
#'
#' @rdname ig_close_session
#' @export
ig_logout <- ig_close_session

#' Null-coalescing operator for internal use
#' @noRd
`%||%` <- function(x, y) if (!is.null(x)) x else y
