#' igfetchr: Lightweight IG Trading REST API wrapper
#'
#' Helper functions to authenticate and fetch market/account data from the IG Trading REST API.
#' All exported functions return tibbles where appropriate. This package is for data access only
#' and not financial advice. Trading CFDs and spread bets carries a high risk of losing money.
#'
#' @keywords internal
"_PACKAGE"

#' Authenticate with IG Trading API
#'
#' Authenticate to the IG Trading REST API and return required tokens for subsequent calls.
#'
#' @param identifier IG account username (character)
#' @param password IG account password (character)
#' @param api_key API key from https://labs.ig.com (character)
#' @param demo Logical; use demo API if TRUE (default TRUE)
#' @return A named list with elements: cst, security, and base_url. Use this list as the `auth` argument
#'   in other functions.
#' @examples
#' \dontrun{
#' auth <- ig_auth("my_user", "my_pass", "my_api_key", demo = TRUE)
#' }
#' @export
ig_auth <- function(identifier, password, api_key, demo = TRUE) {
  stopifnot(is.character(identifier), is.character(password), is.character(api_key))
  base_url <- if (isTRUE(demo)) "https://demo-api.ig.com/gateway/deal" else "https://api.ig.com/gateway/deal"

  # Support offline/mock mode for tests
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    return(list(cst = "mock_cst", security = "mock_security", base_url = base_url))
  }

  body <- jsonlite::toJSON(list(identifier = identifier, password = password), auto_unbox = TRUE)
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

  list(cst = cst, security = security, base_url = base_url)
}

# Internal helper for requests
.ig_request <- function(path, auth, api_key = NULL, method = c("GET", "POST", "DELETE"), query = list(), body = NULL, mock_response = NULL) {
  method <- match.arg(method)
  if (!is.null(mock_response)) {
    # Assume user provided data.frame-like structure or list convertible to tibble
    return(tibble::as_tibble(mock_response))
  }
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    stop("Network calls disabled during tests. Provide `mock_response` or set IGFETCHR_TESTING to '' to enable network.")
  }
  if (is.null(auth) || !is.list(auth) || is.null(auth$base_url)) {
    stop("`auth` must be a list returned from ig_auth() with a base_url element.")
  }
  url <- paste0(auth$base_url, path)
  headers <- httr::add_headers(
    "X-IG-API-KEY" = api_key %||% "",
    "CST" = auth$cst %||% "",
    "X-SECURITY-TOKEN" = auth$security %||% "",
    "Accept" = "application/json"
  )

  resp <- switch(
    method,
    GET = httr::GET(url, headers, query = query),
    POST = httr::POST(url, headers, body = body, encode = "json"),
    DELETE = httr::DELETE(url, headers)
  )
  status <- httr::status_code(resp)
  if (status >= 400) {
    msg <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"), error = function(e) "")
    stop("API request failed (status ", status, "): ", msg)
  }
  content <- httr::content(resp, "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(content, simplifyVector = TRUE)
  # If response contains a top-level list with prices/markets/accounts, try to coerce to tibble
  if (is.data.frame(json)) {
    return(tibble::as_tibble(json))
  }
  # Some IG endpoints wrap data inside fields like "prices" or "markets"
  if (is.list(json) && length(json) == 1 && is.data.frame(json[[1]])) {
    return(tibble::as_tibble(json[[1]]))
  }
  # Fallback: convert lists to tibble when possible
  if (is.list(json)) {
    # try to identify a table-like element
    table_elt <- json[["prices"]] %||% json[["markets"]] %||% json[["accounts"]] %||% json[["positions"]] %||% NULL
    if (!is.null(table_elt) && is.data.frame(table_elt)) {
      return(tibble::as_tibble(table_elt))
    }
  }
  # Return list wrapped as tibble with single row if nothing else
  return(tibble::as_tibble(list(response = json)))
}

#' Search markets
#'
#' Search markets by text query. Returns a tibble of matching markets.
#'
#' @param query Character search string (e.g., "USD/CHF")
#' @param auth Authentication list returned by ig_auth()
#' @param api_key API key (optional; if NULL, must have been used during ig_auth())
#' @param mock_response Optional mock response (for tests); a data.frame or list convertible to tibble
#' @return tibble with market information
#' @examples
#' \dontrun{
#' auth <- ig_auth("user", "pass", "api_key")
#' markets <- ig_search_markets("USD/CHF", auth)
#' }
#' @export
ig_search_markets <- function(query, auth, api_key = NULL, mock_response = NULL) {
  stopifnot(is.character(query))
  path <- paste0("/markets?search=", utils::URLencode(query, reserved = TRUE))
  res <- .ig_request(path, auth = auth, api_key = api_key, method = "GET", mock_response = mock_response)
  tibble::as_tibble(res)
}

#' Get current price for a market
#'
#' Fetch current price(s) for the given market epic.
#'
#' @param epic Market epic (character)
#' @param auth Authentication list from ig_auth()
#' @param api_key API key (optional)
#' @param mock_response Optional mock response (for tests)
#' @return tibble with current price information
#' @examples
#' \dontrun{
#' auth <- ig_auth("user", "pass", "api_key")
#' price <- ig_get_price("CS.D.EURUSD.CFD.IP", auth)
#' }
#' @export
ig_get_price <- function(epic, auth, api_key = NULL, mock_response = NULL) {
  stopifnot(is.character(epic))
  path <- paste0("/prices/", utils::URLencode(epic, reserved = TRUE))
  res <- .ig_request(path, auth = auth, api_key = api_key, method = "GET", mock_response = mock_response)
  tibble::as_tibble(res)
}

#' Get historical prices
#'
#' Fetch historical prices for a market epic between from and to (ISO 8601 or Date-like).
#'
#' @param epic Market epic (character)
#' @param from Start datetime or date as character (optional)
#' @param to End datetime or date as character (optional)
#' @param resolution Character resolution code (e.g., "D" for daily). Defaults to "D".
#' @param auth Authentication list from ig_auth()
#' @param api_key API key (optional)
#' @param mock_response Optional mock response (for tests)
#' @return tibble with historical OHLC data
#' @examples
#' \dontrun{
#' auth <- ig_auth("user", "pass", "api_key")
#' hist <- ig_get_historical("CS.D.EURUSD.CFD.IP", "2020-01-01", "2020-12-31", "D", auth)
#' }
#' @export
ig_get_historical <- function(epic, from = NULL, to = NULL, resolution = "D", auth, api_key = NULL, mock_response = NULL) {
  stopifnot(is.character(epic), is.character(resolution))
  q <- list(resolution = resolution)
  if (!is.null(from)) q$from <- as.character(from)
  if (!is.null(to)) q$to <- as.character(to)
  path <- paste0("/prices/", utils::URLencode(epic, reserved = TRUE), "/history")
  res <- .ig_request(path, auth = auth, api_key = api_key, method = "GET", query = q, mock_response = mock_response)
  tibble::as_tibble(res)
}

#' Get accounts and positions
#'
#' Retrieve account summaries and open positions for the authenticated user.
#'
#' @param auth Authentication list from ig_auth()
#' @param api_key API key (optional)
#' @param mock_response Optional mock response (for tests)
#' @return tibble or list depending on endpoint structure
#' @examples
#' \dontrun{
#' auth <- ig_auth("user", "pass", "api_key")
#' accounts <- ig_get_accounts(auth)
#' }
#' @export
ig_get_accounts <- function(auth, api_key = NULL, mock_response = NULL) {
  path <- "/accounts"
  res <- .ig_request(path, auth = auth, api_key = api_key, method = "GET", mock_response = mock_response)
  tibble::as_tibble(res)
}

#' Get options / derivative positions
#'
#' Retrieve positions filtered for options/derivatives. Returns a tibble with position details.
#'
#' @param auth Authentication list from ig_auth()
#' @param api_key API key (optional)
#' @param mock_response Optional mock response (for tests); data.frame or list convertible to tibble
#' @return tibble with options position details (dealId, size, direction, instrument, etc.)
#' @examples
#' \dontrun{
#' auth <- ig_auth("user", "pass", "api_key")
#' options <- ig_get_options(auth)
#' }
#' @export
ig_get_options <- function(auth, api_key = NULL, mock_response = NULL) {
  # Using the /positions endpoint; filter may be applied client-side for options
  path <- "/positions"
  res <- .ig_request(path, auth = auth, api_key = api_key, method = "GET", mock_response = mock_response)
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
#' Place a market trade (BUY/SELL) for an epic. For tests use `mock_response`.
#'
#' @param epic Market epic (character)
#' @param direction "BUY" or "SELL" (character)
#' @param size Numeric size (units)
#' @param auth Authentication list from ig_auth()
#' @param api_key API key (optional)
#' @param limit Optional numeric limit price
#' @param stop Optional numeric stop price
#' @param mock_response Optional mock response (data.frame or list) to avoid network in tests
#' @return tibble with trade confirmation (dealId, status, dealReference, etc.)
#' @examples
#' \dontrun{
#' auth <- ig_auth("user", "pass", "api_key")
#' res <- ig_execute_trade("CS.D.EURUSD.CFD.IP", "BUY", 1.0, auth)
#' }
#' @export
ig_execute_trade <- function(epic, direction, size, auth, api_key = NULL, limit = NULL, stop = NULL, mock_response = NULL) {
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
    size = size
  )
  if (!is.null(limit)) body$limit <- limit
  if (!is.null(stop)) body$stop <- stop

  res <- .ig_request(url_path, auth = auth, api_key = api_key, method = "POST", body = body)
  tibble::as_tibble(res)
}

#' Close session / logout
#'
#' Close the authenticated session. Alias: ig_close_session is provided for clarity.
#'
#' @param auth Authentication list from ig_auth()
#' @param api_key API key (optional)
#' @return Logical TRUE if the session was closed successfully.
#' @examples
#' \dontrun{
#' auth <- ig_auth("user", "pass", "api_key")
#' ig_close_session(auth)
#' }
#' @export
ig_logout <- function(auth, api_key = NULL) {
  if (identical(Sys.getenv("IGFETCHR_TESTING"), "true")) {
    return(invisible(TRUE))
  }
  if (is.null(auth) || !is.list(auth)) {
    stop("`auth` must be a list returned from ig_auth()")
  }
  url <- paste0(auth$base_url, "/session")
  headers <- httr::add_headers(
    "X-IG-API-KEY" = api_key %||% "",
    "CST" = auth$cst %||% "",
    "X-SECURITY-TOKEN" = auth$security %||% ""
  )
  resp <- httr::DELETE(url, headers)
  status <- httr::status_code(resp)
  if (status >= 400) {
    msg <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"), error = function(e) "")
    stop("Logout failed (status ", status, "): ", msg)
  }
  invisible(TRUE)
}

#' Close session / logout (alias)
#'
#' Convenience wrapper for ig_logout()
#'
#' @param auth Authentication list from ig_auth()
#' @param api_key API key (optional)
#' @return Logical TRUE if the session was closed successfully.
#' @export
ig_close_session <- function(auth, api_key = NULL) {
  ig_logout(auth = auth, api_key = api_key)
}

#' Null-coalescing operator for internal use
#' @noRd
`%||%` <- function(x, y) if (!is.null(x)) x else y
