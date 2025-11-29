# Retrieve IG account details

Fetches details of IG accounts associated with the authenticated
session. Returns a tibble containing account information such as account
ID, name, balance, and status.

## Usage

``` r
ig_get_accounts(auth, mock_response = NULL)
```

## Arguments

- auth:

  List. Authentication details from \`ig_auth()\`, including \`cst\`,
  \`security\`, \`base_url\`, \`api_key\`, and \`acc_number\`.

- mock_response:

  List or data frame. Optional mock response for testing, bypassing the
  API call.

## Value

A tibble with account information including account ID, name, balance
and currency.

## Examples

``` r
if (FALSE) { # \dontrun{
# Authenticate and get accounts
auth <- ig_auth(
  username = "your_username",
  password = "your_password",
  api_key = "your_api_key",
  acc_type = "DEMO",
  acc_number = "ABC123"
)
accounts <- ig_get_accounts(auth)
print(accounts)

# Using mock response for testing
mock_response <- data.frame(
  accountId = "ABC123",
  accountName = "Demo Account",
  balance.balance = 10000,
  currency = "SEK"
)
accounts <- ig_get_accounts(auth, mock_response = mock_response)
} # }
```
