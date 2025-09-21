# igfetchr Development TODO

This file tracks the high-level steps required to implement the igfetchr R package and prepare it for CRAN submission.

- [x] Analyze requirements and prepare plan
- [ ] Create package skeleton (DESCRIPTION, NAMESPACE, R/, man/, vignettes/, tests/)
- [ ] Implement core functions (5–8 functions):
  - ig_auth()
  - ig_search_markets()
  - ig_get_price()
  - ig_get_historical()
  - ig_get_accounts()
  - ig_close_session() or ig_logout()
- [ ] Add internal HTTP helper(s) with robust error handling (rate limits, invalid keys)
- [ ] Depend on minimal packages: httr, jsonlite, tibble; suggest lubridate
- [ ] Add roxygen2 documentation for every exported function (including risk disclaimer)
- [ ] Add package-level documentation (?igfetchr) and vignette ("getting-started")
- [ ] Add unit tests with testthat (mock responses; no internet during checks)
  - Achieve >= 80% coverage
- [ ] Add examples (use \\dontrun{} for real API calls or use mock mode)
- [ ] Add README.md, NEWS.md, cran-comments.md and .Rbuildignore
- [ ] Run devtools::check() and rhub::check_for_cran(); fix all errors/warnings/notes
- [ ] Test on Windows (win-builder or devtools::check_win_release())
- [ ] Finalize version, build, and submit to CRAN

Notes / conventions
- License: MIT
- Author: Saw Simeon
- CRAN disclaimer to include in DESCRIPTION and each exported function:
  "Trading CFDs and spread bets carries high risk of losing money. This package is for data access, not financial advice."
- Testing strategy: offer a mock mode via environment variable (IGFETCHR_TESTING = "true") so tests run offline and deterministically.

Next steps
1. Create package skeleton and essential files.
2. Implement initial function stubs with roxygen headers (ig_auth + 4 data functions).
3. Add offline mock-mode behavior for functions so tests do not require internet.
4. Write tests and vignette.
