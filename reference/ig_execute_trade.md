# Execute a trade (place OTC position)

Places a market trade using the IG API. If stops/limits fail, falls back
to placing the trade without them and adding via PUT.

## Usage

``` r
ig_execute_trade(
  epic,
  direction,
  size,
  auth,
  currency_code = NULL,
  expiry = NULL,
  guaranteed_stop = FALSE,
  level = NULL,
  time_in_force = "FILL_OR_KILL",
  order_type = "MARKET",
  limit_distance = NULL,
  limit_level = NULL,
  stop_distance = NULL,
  stop_level = NULL,
  deal_reference = NULL,
  force_open = NULL,
  mock_response = NULL
)
```

## Arguments

- epic:

  Character. Market epic (e.g., "CS.D.USDCHF.MINI.IP").

- direction:

  Character. "BUY" or "SELL".

- size:

  Numeric. Trade size (units).

- auth:

  List. Authentication details from
  [`ig_auth`](https://sawsimeon.github.io/igfetchr/reference/ig_auth.md).

- currency_code:

  Character. Currency code (e.g., "CHF"). Defaults to NULL.

- expiry:

  Character. Expiry date (e.g., "-"). Defaults to NULL.

- guaranteed_stop:

  Logical. Use guaranteed stop. Defaults to FALSE.

- level:

  Numeric. Price level for LIMIT orders. Defaults to NULL.

- time_in_force:

  Character. "EXECUTE_AND_ELIMINATE" or "FILL_OR_KILL". Defaults to
  "FILL_OR_KILL".

- order_type:

  Character. "MARKET" or "LIMIT". Defaults to "MARKET".

- limit_distance:

  Numeric. Limit distance in points. Defaults to NULL.

- limit_level:

  Numeric. Limit price. Defaults to NULL.

- stop_distance:

  Numeric. Stop distance in points. Defaults to NULL.

- stop_level:

  Numeric. Stop price. Defaults to NULL.

- deal_reference:

  Character. Custom deal reference. Defaults to NULL.

- force_open:

  Logical. Force new position. Defaults to TRUE if stops/limits
  specified.

- mock_response:

  List or data frame. Mock response for testing.

## Value

A tibble with trade confirmation details including deal ID and deal
reference.

## Examples

``` r
if (FALSE) { # \dontrun{
auth <- ig_auth(
username = "your_username",
password = "your_password",
api_key = "your_api_key",
acc_type = "DEMO",
acc_number = "ABC123")
res <- ig_execute_trade(
  epic = "CS.D.USDCHF.MINI.IP",
  direction = "BUY",
  size = 1.0,
  auth = auth,
  currency_code = "CHF",
  order_type = "MARKET",
  time_in_force = "FILL_OR_KILL",
  limit_distance = 2000,
  stop_distance = 2000,
  guaranteed_stop = FALSE,
  force_open = TRUE
)
print(res)
} # }
```
