# Get options/derivatives positions

Retrieves positions filtered for options/derivatives from the IG API.
Returns a tibble with position details.

## Usage

``` r
ig_get_options(auth, mock_response = NULL)
```

## Arguments

- auth:

  List. Authentication details from \`ig_auth()\`, including \`cst\`,
  \`security\`, \`base_url\`, \`api_key\`, and \`acc_number\`.

- mock_response:

  List or data frame. Optional mock response for testing, bypassing the
  API call.

## Value

A tibble with options/derivative position details including deal ID,
size, direction and instrument type.

## Examples

``` r
if (FALSE) { # \dontrun{
# Authenticate and get options positions
auth <- ig_auth(
  username = "your_username",
  password = "your_password",
  api_key = "your_api_key",
  acc_type = "DEMO",
  acc_number = "ABC123"
)
options <- ig_get_options(auth)
print(options)

# Using mock response for testing
mock_response <- data.frame(
  dealId = "DIXXXX",
  size = 1.5,
  direction = "BUY",
  instrumentType = "OPTION"
)
options <- ig_get_options(auth, mock_response = mock_response)
} # }
```
