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
#' @param mock_response List or data frame. Optional mock response for testing.
#'
#' @return A tibble containing the API response data, or a tibble with a single column
#' containing the raw JSON response if the response cannot be converted to a data frame.
#'
#' @keywords internal
.ig_request <- function(path, auth, method = c("GET", "POST", "DELETE"), query = list(), body = NULL, mock_response = NULL) {
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
  if (is.data.frame(json)) {
    return(tibble::as_tibble(json))
  }
  if (is.list(json) && length(json) == 1 && is.data.frame(json[[1]])) {
    return(tibble::as_tibble(json[[1]]))
  }
  if (is.list(json)) {
    table_elt <- json[["prices"]] %||% json[["markets"]] %||% json[["accounts"]] %||% json[["positions"]] %||% NULL
    if (!is.null(table_elt) && is.data.frame(table_elt)) {
      return(tibble::as_tibble(table_elt))
    }
  }
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
#' mock_response <- data.frame(
#'   marketStatus = "TRADEABLE",
#'   bid = 0.798,
#'   offer = 0.798,
#'   high = 0.801,
#'   low = 0.797,
#'   updateTime = "2025/09/26 21:58:57"
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
  res <- .ig_request(
    path = path,
    auth = auth,
    method = "GET",
    mock_response = mock_response
  )
  
  # Return as tibble
  tibble::as_tibble(res)
}

#' Get historical prices for a market
#'
#' Fetches historical prices for a market epic between specified dates at a given resolution from the IG API.
#'
#' @param epic Character. Market epic (e.g., "CS.D.USDCHF.MINI.IP").
#' @param from Character or Date. Start datetime (e.g., "2025-09-26T00:00:00.000+0000"). Required.
#' @param to Character or Date. End datetime (e.g., "2025-09-26T23:59:59.999+0000"). Required.
#' @param resolution Character. Resolution code (e.g., "MINUTE", "HOUR", "DAY"). Defaults to "DAY".
#' @param auth List. Authentication details from `ig_auth()`, including `cst`, `security`, `base_url`, `api_key`, and `acc_number`.
#' @param mock_response List or data frame. Optional mock response for testing, bypassing the API call.
#'
#' @return A tibble with historical OHLC data, including columns like `snapshotTime`, `openPrice`, `highPrice`, `lowPrice`, `closePrice`, and others as returned by the IG API `/prices/{epic}/{resolution}/{from}/{to}` endpoint.
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
#'   from = "2025-09-26T00:00:00.000+0000",
#'   to = "2025-09-26T23:59:59.999+0000",
#'   resolution = "MINUTE",
#'   auth
#' )
#' print(hist)
#'
#' # Using mock response for testing
#' mock_response <- data.frame(
#'   snapshotTime = "2025/09/26 00:00:00",
#'   openPrice = 0.798,
#'   highPrice = 0.801,
#'   lowPrice = 0.797,
#'   closePrice = 0.798
#' )
#' hist <- ig_get_historical(
#'   "CS.D.USDCHF.MINI.IP",
#'   from = "2025-09-26T00:00:00.000+0000",
#'   to = "2025-09-26T23:59:59.999+0000",
#'   resolution = "MINUTE",
#'   auth,
#'   mock_response = mock_response
#' )
#' }
#'
#' @export
ig_get_historical <- function(epic, from, to, resolution = "DAY", auth, mock_response = NULL) {
  stopifnot(
    is.character(epic), nchar(epic) > 0,
    is.character(resolution), resolution %in% c("SECOND", "MINUTE", "HOUR", "DAY", "WEEK", "MONTH"),
    is.character(from) || inherits(from, "Date"),
    is.character(to) || inherits(to, "Date")
  )
  
  # Format dates
  from <- if (inherits(from, "Date")) format(from, "%Y-%m-%dT00:00:00.000+0000") else from
  to <- if (inherits(to, "Date")) format(to, "%Y-%m-%dT23:59:59.999+0000") else to
  
  # Construct path for historical prices
  path <- paste0(
    "/prices/", utils::URLencode(epic, reserved = TRUE), "/",
    resolution, "/", from, "/", to
  )
  
  # Call .ig_request()
  res <- .ig_request(
    path = path,
    auth = auth,
    method = "GET",
    query = list(),
    mock_response = mock_response
  )
  
  # Return as tibble
  tibble::as_tibble(res)
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
