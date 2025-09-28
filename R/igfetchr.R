#' igfetchr: Lightweight IG Trading REST API wrapper
#'
#' Helper functions to authenticate and fetch market/account data from the IG Trading REST API.
#' All exported functions return tibbles where appropriate. This package is for data access only
#' and not financial advice. Trading CFDs and spread bets carries a high risk of losing money.
#'
#' @keywords internal
"_PACKAGE"

#' Authenticate with the IG API
#'
#' Authenticates with the IG API to obtain session tokens (CST and X-SECURITY-TOKEN)
#' for subsequent API requests. Supports environment variables for credentials and
#' optional account type and number.
#'
#' @param username Character. IG account username. Defaults to `IG_SERVICE_USERNAME` environment variable.
#' @param password Character. IG account password. Defaults to `IG_SERVICE_PASSWORD` environment variable.
#' @param api_key Character. IG API key. Defaults to `IG_SERVICE_API_KEY` environment variable.
#' @param acc_type Character. Account type, either "DEMO" or "LIVE". Defaults to "DEMO".
#' @param acc_number Character. Optional account number. Defaults to `IG_SERVICE_ACC_NUMBER` or NULL.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{cst}: Client session token.
#'   \item \code{security}: Security token (X-SECURITY-TOKEN).
#'   \item \code{base_url}: Base URL for API requests (DEMO or LIVE).
#'   \item \code{api_key}: API key used for authentication.
#'   \item \code{acc_type}: Account type ("DEMO" or "LIVE").
#'   \item \code{acc_number}: Account number (or NULL if not provided).
#' }
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
  username <- if (nchar(username) > 0) username else stop("`username` must be provided via IG_SERVICE_USERNAME or function argument.")
  password <- if (nchar(password) > 0) password else stop("`password` must be provided via IG_SERVICE_PASSWORD or function argument.")
  api_key <- if (nchar(api_key) > 0) api_key else stop("`api_key` must be provided via IG_SERVICE_API_KEY or function argument.")
  acc_type <- if (nchar(acc_type) > 0) acc_type else "DEMO"  # Default to DEMO if not set
  acc_number <- if (nchar(acc_number) > 0) acc_number else NULL  # acc_number is optional

  stopifnot(is.character(username), is.character(password), is.character(api_key))
  if (!is.character(acc_type) || length(acc_type) != 1) stop("`acc_type` must be a single character, e.g. 'DEMO' or 'LIVE'")
  acc_type_upper <- toupper(acc_type)
  base_url <- if (identical(acc_type_upper, "DEMO")) "https://demo-api.ig.com/gateway/deal" else "https://api.ig.com/gateway/deal"

  # Support offline/mock mode for tests
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    return(list(cst = "mock_cst", security = "mock_security", base_url = base_url, api_key = api_key, acc_type = acc_type_upper, acc_number = acc_number))
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

#' Internal function to make IG API requests
#'
#' Makes HTTP requests to the IG API using authentication details from `ig_auth()`.
#' Handles GET, POST, and DELETE methods and processes JSON responses into tibbles.
#' Not intended for direct use; called by higher-level functions like `ig_get_accounts()`.
#'
#' @param path Character. API endpoint path (e.g., "/accounts").
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param method Character. HTTP method ("GET", "POST", or "DELETE"). Defaults to "GET".
#' @param query List. Optional query parameters for GET requests.
#' @param body List. Optional request body for POST requests.
#' @param version Character. API version ("1", "2", or "3"). If NULL, no VERSION header is sent. Defaults to NULL.
#' @param mock_response List or data frame. Optional mock response for testing.
#'
#' @return A tibble containing the API response data, or a tibble with a single column
#' containing the raw JSON response if the response cannot be converted to a data frame.
#'
#' @keywords internal
.ig_request <- function(path, auth, method = c("GET", "POST", "DELETE"), query = list(), body = NULL, version = NULL, mock_response = NULL) {
  method <- match.arg(method)
  if (!is.null(mock_response)) {
    return(tibble::as_tibble(mock_response))
  }
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    stop("Network calls disabled during tests. Provide `mock_response` or set IGFETCHR_TESTING to '' to enable network.")
  }
  if (is.null(auth) || !is.list(auth) || is.null(auth$base_url)) {
    stop("`auth` must be a list returned from ig_auth() with a base_url element.")
  }
  if (is.null(auth$api_key) || !is.character(auth$api_key) || nchar(auth$api_key) == 0) {
    stop("`auth` must contain a non-empty api_key.")
  }

  url <- paste0(auth$base_url, path)

  # Construct headers in a single call
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

  # Execute request with tryCatch
  resp <- tryCatch({
    switch(
      method,
      GET = httr::GET(url, headers, query = query),
      POST = httr::POST(url, headers, body = body, encode = "json"),
      DELETE = httr::DELETE(url, headers)
    )
  }, error = function(e) {
    stop("Failed to execute HTTP request: ", e$message)
  })

  # Verify response is a valid httr response object
  if (!inherits(resp, "response")) {
    stop("Invalid response object returned by httr. Check network connection or API availability.")
  }

  status <- httr::status_code(resp)
  if (status >= 400) {
    msg <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"), error = function(e) "Unknown error")
    stop("API request failed (status ", status, "): ", msg)
  }

  content <- httr::content(resp, "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(content, simplifyVector = TRUE)

  # Handle different response structures
  if (is.data.frame(json)) {
    return(tibble::as_tibble(json))
  }
  if (is.list(json) && length(json) == 1 && is.data.frame(json[[1]])) {
    return(tibble::as_tibble(json[[1]]))
  }
  if (is.list(json)) {
    table_elt <- json[["prices"]] %||% json[["markets"]] %||% json[["accounts"]] %||% json[["positions"]] %||% json[["snapshot"]] %||% NULL
    if (!is.null(table_elt)) {
      if (is.data.frame(table_elt)) {
        return(tibble::as_tibble(table_elt))
      }
      if (is.list(table_elt)) {
        # Convert NULL to NA to avoid tibble errors
        table_elt <- lapply(table_elt, function(x) if (is.null(x)) NA else x)
        return(tibble::as_tibble(table_elt))
      }
    }
  }
  warning("Unexpected response structure: ", toString(names(json)))
  return(tibble::as_tibble(list(response = json)))
}

#' Search markets
#'
#' Search markets by text query. Returns a tibble of matching markets from the IG API.
#'
#' @param query Character. Search string for markets (e.g., "USD/CHF").
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with market information, including columns like `epic`, `instrumentName`, `marketStatus`, and others as returned by the IG API `/markets` endpoint.
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
#' # Using mock response for testing
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
  
  # Return as tibble
  tibble::as_tibble(res)
}

#' Get current price for a market
#'
#' Fetches current price(s) for the given market epic from the IG API using the `/markets/{epic}` endpoint.
#'
#' @param epic Character. Market epic (e.g., "CS.D.USDCHF.CFD.IP").
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with price information, including columns like `marketStatus`, `bid`, `offer`, `high`, `low`, `updateTime`, and others as returned by the IG API `/markets/{epic}` endpoint.
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
  
  # Check if response is a nested list (unexpected structure)
  if ("response" %in% names(res)) {
    warning("Unexpected response structure for epic '", epic, "': ", toString(names(res$response)))
    return(tibble::tibble(response = list(res$response)))
  }
  
  # Return as tibble
  tibble::as_tibble(res)
}

#' Get historical prices for a market
#'
#' Fetches historical prices for a market epic between specified dates at a given resolution from the IG API.
#' Uses the /prices/{epic} endpoint (version 3) with pagination, with a fallback to version 2.
#'
#' @param epic Character. Market epic (e.g., "CS.D.USDCHF.MINI.IP").
#' @param from Character or Date. Start date (e.g., "2025-08-01" or "2025-08-01T00:00:00+00:00"). Required.
#' @param to Character or Date. End date (e.g., "2025-08-26" or "2025-08-26T23:59:59+00:00"). Required.
#' @param resolution Character. Resolution code (e.g., "D", "DAY", "1MIN", "HOUR"). Defaults to "DAY".
#' @param page_size Integer. Number of data points per page (v3 only). Defaults to 20.
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#' @param wait Numeric. Seconds to wait between paginated API calls (v3 only). Defaults to 1.
#'
#' @return A tibble with historical OHLC data, including columns like `snapshotTime`, `openPrice`, `highPrice`, `lowPrice`, `closePrice`, and others as returned by the IG API.
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
#'   from = "2025-08-01",
#'   to = "2025-08-26",
#'   resolution = "DAY",
#'   auth
#' )
#' print(hist)
#'
#' # Using ISO 8601 format
#' hist <- ig_get_historical(
#'   "CS.D.USDCHF.MINI.IP",
#'   from = "2025-08-01T00:00:00+00:00",
#'   to = "2025-08-26T23:59:59+00:00",
#'   resolution = "DAY",
#'   auth
#' )
#'
#' # Using mock response for testing
#' mock_response <- list(
#'   prices = data.frame(
#'     snapshotTime = "2025/08/01 00:00:00",
#'     openPrice = 0.970,
#'     highPrice = 0.975,
#'     lowPrice = 0.965,
#'     closePrice = 0.971
#'   ),
#'   metadata = list(allowance = list(remainingAllowance = 10000), pageData = list(pageNumber = 1, totalPages = 1))
#' )
#' hist <- ig_get_historical(
#'   "CS.D.USDCHF.MINI.IP",
#'   from = "2025-08-01",
#'   to = "2025-08-26",
#'   resolution = "DAY",
#'   auth,
#'   mock_response = mock_response
#' )
#' }
#'
#' @export
ig_get_historical <- function(epic, from, to, resolution = "DAY", page_size = 20, auth, mock_response = NULL, wait = 1) {
  stopifnot(
    is.character(epic), nchar(epic) > 0,
    is.character(from) || inherits(from, "Date"),
    is.character(to) || inherits(to, "Date"),
    is.character(resolution),
    is.numeric(page_size), page_size > 0,
    is.numeric(wait), wait >= 0
  )
  
  # Validate epic
  if (is.null(mock_response)) {
    markets <- tryCatch(
      {
        .ig_request("/markets", auth, method = "GET")
      },
      error = function(e) {
        warning("Failed to fetch markets to validate epic: ", e$message)
        return(NULL)
      }
    )
    if (!is.null(markets) && is.data.frame(markets) && !any(markets$epic == epic)) {
      warning("Epic '", epic, "' not found in available markets. Available epics: ", paste(markets$epic, collapse = ", "))
    }
  }
  
  # Map resolution codes (adapted from Python conv_resol)
  resolution_map <- c(
    "1s" = "SECOND",
    "1Min" = "MINUTE", "M" = "MINUTE", "MINUTE" = "MINUTE",
    "2Min" = "MINUTE_2", "3Min" = "MINUTE_3", "5Min" = "MINUTE_5",
    "10Min" = "MINUTE_10", "15Min" = "MINUTE_15", "30Min" = "MINUTE_30",
    "1h" = "HOUR", "H" = "HOUR", "HOUR" = "HOUR",
    "2h" = "HOUR_2", "3h" = "HOUR_3", "4h" = "HOUR_4",
    "D" = "DAY", "DAY" = "DAY",
    "W" = "WEEK", "WEEK" = "WEEK",
    "ME" = "MONTH", "MONTH" = "MONTH"
  )
  if (!resolution %in% names(resolution_map)) {
    warning("Invalid resolution '", resolution, "', returning as-is. Supported: ", paste(names(resolution_map), collapse=", "))
    resolution <- resolution
  } else {
    resolution <- resolution_map[resolution]
  }
  
  # Parse and validate dates
  from_date <- if (inherits(from, "Date")) from else as.Date(from, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%dT%H:%M:%S%z"))
  to_date <- if (inherits(to, "Date")) to else as.Date(to, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%dT%H:%M:%S%z"))
  if (is.na(from_date) || is.na(to_date)) {
    stop("Invalid date format for 'from' or 'to'. Use YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS+00:00.")
  }
  if (to_date < from_date) {
    stop("'to' date must be after 'from' date.")
  }
  
  # Check date range (max 90 days for DEMO accounts)
  if (as.numeric(to_date - from_date) > 90) {
    warning("Date range exceeds 90 days, which may not be supported in DEMO accounts. Consider a shorter range.")
  }
  # Warn if dates are in the future
  current_date <- Sys.Date()
  if (from_date > current_date || to_date > current_date) {
    warning("Dates are in the future, which may not be supported in DEMO accounts. Current date: ", current_date)
  }
  
  # Convert dates (adapted from Python conv_datetime)
  conv_datetime <- function(dt, version = "2") {
    dt_parsed <- tryCatch({
      if (inherits(dt, "Date")) {
        as.POSIXct(dt)
      } else {
        as.POSIXct(dt, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%dT%H:%M:%S%z"))
      }
    }, error = function(e) {
      warning("Date parsing failed, returning original: ", dt)
      return(dt)
    })
    if (inherits(dt_parsed, "character")) return(dt)
    fmt <- if (version == "1") "%Y:%m:%d-%H:%M:%S" else "%Y/%m/%d %H:%M:%S"
    format(dt_parsed, fmt)
  }
  
  # Format dates for v3 and v2
  from_v3 <- conv_datetime(from_date, version = "3")
  to_v3 <- conv_datetime(to_date, version = "3")
  from_v2 <- format(from_date, "%Y-%m-%d")
  to_v2 <- format(to_date, "%Y-%m-%d")
  
  # Construct path and initial query for v3
  path <- paste0("/prices/", utils::URLencode(epic, reserved = TRUE))
  prices <- list()
  page_number <- 1
  more_results <- TRUE
  
  while (more_results) {
    query <- list(
      resolution = resolution,
      from = from_v3,
      to = to_v3,
      pageSize = page_size,
      pageNumber = page_number
    )
    
    # Call .ig_request() for v3
    res <- tryCatch(
      {
        .ig_request(
          path = path,
          auth = auth,
          method = "GET",
          query = query,
          version = "3",
          mock_response = if (page_number == 1) mock_response else NULL
        )
      },
      error = function(e) {
        message("v3 API request failed: ", e$message, "\nRequested URL: ", path, " with query: ", paste(names(query), query, sep="=", collapse="&"))
        # Fallback to v2 endpoint: /prices/{epic}/{resolution}/{startDate}/{endDate}
        path_v2 <- paste0("/prices/", utils::URLencode(epic, reserved = TRUE), "/", resolution, "/", from_v2, "/", to_v2)
        message("Trying fallback v2 endpoint: ", path_v2)
        res_v2 <- tryCatch(
          {
            .ig_request(
              path = path_v2,
              auth = auth,
              method = "GET",
              query = list(),
              version = "2",
              mock_response = if (page_number == 1) mock_response else NULL
            )
          },
          error = function(e2) {
            message("v2 API request failed: ", e2$message, "\nRequested URL: ", path_v2)
            stop("Both v3 and v2 API requests failed for epic '", epic, "': ", e$message, " (v3), ", e2$message, " (v2). Check epic validity or contact IG support at labs.ig.com.")
          }
        )
        return(res_v2)
      }
    )
    
    # Extract prices and metadata
    if (!is.null(res$prices)) {
      prices <- append(prices, res$prices)
    } else {
      message("No 'prices' field in response: ", toString(names(res)))
    }
    page_data <- res$metadata$pageData
    if (is.null(page_data) || page_data$totalPages == 0 || page_data$pageNumber == page_data$totalPages) {
      more_results <- FALSE
    } else {
      page_number <- page_number + 1
      Sys.sleep(wait)
    }
  }
  
  # Combine results into a tibble
  if (length(prices) == 0) {
    warning("No prices returned from API for epic '", epic, "'. Verify epic and date range with IG support at labs.ig.com.")
    return(tibble::tibble())
  }
  result <- tibble::as_tibble(do.call(rbind, lapply(prices, as.data.frame)))
  attr(result, "metadata") <- res$metadata
  return(result)
}

#' Retrieve IG account details
#'
#' Fetches details of IG accounts associated with the authenticated session.
#' Returns a tibble containing account information such as account ID, name, balance, and status.
#'
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with columns including `accountId`, `accountName`, `balance`, `currency`, and others as returned by the IG API `/accounts` endpoint.
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
  res <- .ig_request(
    path = path,
    auth = auth,
    method = "GET",
    mock_response = mock_response
  )
  
  # Return as tibble (rely on .ig_request()'s handling for structure)
  tibble::as_tibble(res)
}

#' Get options/derivative positions
#'
#' Retrieves positions filtered for options/derivatives from the IG API. Returns a tibble with position details.
#'
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with options/derivative position details, including columns like `dealId`, `size`, `direction`, `instrumentType`, and others as returned by the IG API `/positions` endpoint. If `instrumentType` is available, filters for "OPTION", "DERIVATIVE", or "OPTION_CONTRACT"; otherwise, returns all positions.
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
  # Using the /positions endpoint; filter may be applied client-side for options
  path <- "/positions"
  res <- .ig_request(
    path = path,
    auth = auth,
    method = "GET",
    mock_response = mock_response
  )
  df <- tibble::as_tibble(res)
  # Try to filter for option-like instruments if a column exists (best-effort)
  if ("instrumentType" %in% names(df)) {
    opts <- df[df$instrumentType %in% c("OPTION", "DERIVATIVE", "OPTION_CONTRACT"), , drop = FALSE]
    return(tibble::as_tibble(opts))
  }
  # If no instrumentType column, return the full positions tibble (caller can filter)
  tibble::as_tibble(df)
}

#' Execute a trade (place OTC position)
#'
#' Places a market trade (BUY/SELL) for a specified market epic. For testing, use `mock_response` to avoid network calls.
#'
#' @param epic Character. Market epic (e.g., "CS.D.USDCHF.CFD.IP").
#' @param direction Character. Trade direction, either "BUY" or "SELL".
#' @param size Numeric. Trade size (units).
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param limit Numeric. Optional limit price for the trade.
#' @param stop Numeric. Optional stop price for the trade.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with trade confirmation details, including columns like `dealId`, `dealReference`, `status`, and others as returned by the IG API `/positions/otc` endpoint.
#'
#' @examples
#' \dontrun{
#' # Authenticate and execute a trade
#' auth <- ig_auth(
#'   username = "your_username",
#'   password = "your_password",
#'   api_key = "your_api_key",
#'   acc_type = "DEMO",
#'   acc_number = "ABC123"
#' )
#' res <- ig_execute_trade("CS.D.USDCHF.CFD.IP", "BUY", 1.0, auth, limit = 0.855, stop = 0.845)
#' print(res)
#'
#' # Using mock response for testing
#' mock_response <- data.frame(
#'   dealId = "DIXXXX",
#'   dealReference = "REF123",
#'   status = "OPEN"
#' )
#' res <- ig_execute_trade("CS.D.USDCHF.CFD.IP", "BUY", 1.0, auth, mock_response = mock_response)
#' }
#'
#' @export
ig_execute_trade <- function(epic, direction, size, auth, limit = NULL, stop = NULL, mock_response = NULL) {
  stopifnot(is.character(epic), is.character(direction), is.numeric(size))
  if (!direction %in% c("BUY", "SELL")) stop("`direction` must be 'BUY' or 'SELL'")

  # Allow tests to bypass network
  if (!is.null(mock_response)) {
    return(tibble::as_tibble(mock_response))
  }
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    stop("Network calls disabled during tests. Provide `mock_response` to simulate a trade.")
  }

  if (is.null(auth) || !is.list(auth) || is.null(auth$base_url)) {
    stop("`auth` must be a list returned from ig_auth() with a base_url element.")
  }

  url_path <- "/positions/otc"
  body <- list(
    epic = epic,
    direction = direction,
    size = size,
    orderType = "MARKET",  # Default to market order
    currencyCode = "USD"   # Default currency, adjust as needed
  )
  if (!is.null(limit)) body$limitLevel <- limit
  if (!is.null(stop)) body$stopLevel <- stop

  res <- .ig_request(
    path = url_path,
    auth = auth,
    method = "POST",
    body = body,
    mock_response = mock_response
  )
  tibble::as_tibble(res)
}

#' Close session
#'
#' Closes the authenticated IG API session.
#'
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response Logical. Optional mock response for testing (returns `TRUE` if provided). Defaults to `NULL`.
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
  res <- .ig_request(
    path = path,
    auth = auth,
    method = "DELETE",
    mock_response = mock_response
  )
  
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
