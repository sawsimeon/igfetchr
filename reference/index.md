# Package index

## Authentication

Functions for IG API authentication

- [`ig_auth()`](https://sawsimeon.github.io/igfetchr/reference/ig_auth.md)
  : Authenticate with the IG API
- [`ig_close_session()`](https://sawsimeon.github.io/igfetchr/reference/ig_close_session.md)
  [`ig_logout()`](https://sawsimeon.github.io/igfetchr/reference/ig_close_session.md)
  : Close session

## Market Data

Functions for markets, prices, and historical data

- [`ig_get_price()`](https://sawsimeon.github.io/igfetchr/reference/ig_get_price.md)
  : Get current price for a market
- [`ig_get_historical()`](https://sawsimeon.github.io/igfetchr/reference/ig_get_historical.md)
  : Get historical prices for a market
- [`ig_search_markets()`](https://sawsimeon.github.io/igfetchr/reference/ig_search_markets.md)
  : Search markets
- [`ig_get_markets_by_epic()`](https://sawsimeon.github.io/igfetchr/reference/ig_get_markets_by_epic.md)
  : Get market details for one or more epics

## Trading & Accounts

Functions for trades and account info

- [`ig_execute_trade()`](https://sawsimeon.github.io/igfetchr/reference/ig_execute_trade.md)
  : Execute a trade (place OTC position)
- [`ig_get_accounts()`](https://sawsimeon.github.io/igfetchr/reference/ig_get_accounts.md)
  : Retrieve IG account details
- [`ig_get_options()`](https://sawsimeon.github.io/igfetchr/reference/ig_get_options.md)
  : Get options/derivatives positions

## Package

General package information

- [`igfetchr`](https://sawsimeon.github.io/igfetchr/reference/igfetchr.md)
  : igfetchr: Lightweight IG Trading REST API wrapper

## Internal Helpers

Low-level helper functions for API requests

- [`.ig_request()`](https://sawsimeon.github.io/igfetchr/reference/dot-ig_request.md)
  : Internal function to make HTTP requests to IG API
