# Close session

Closes the authenticated IG API session.

Alias for \`ig_close_session()\`.

## Usage

``` r
ig_close_session(auth, mock_response = NULL)

ig_logout(auth, mock_response = NULL)
```

## Arguments

- auth:

  List. Authentication details from \`ig_auth()\`, including \`cst\`,
  \`security\`, \`base_url\`, \`api_key\`, and \`acc_number\`.

- mock_response:

  Logical. Optional mock response for testing (returns \`TRUE\` if
  provided). Defaults to \`NULL\`.

## Value

Logical \`TRUE\` if the session was closed successfully.

## Examples

``` r
if (FALSE) { # \dontrun{
# Authenticate and close session
auth <- ig_auth(
  username = "your_username",
  password = "your_password",
  api_key = "your_api_key",
  acc_type = "DEMO",
  acc_number = "ABC123"
)
ig_close_session(auth)

# Using mock response for testing
ig_close_session(auth, mock_response = TRUE)
} # }
```
