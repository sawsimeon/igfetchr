## CRAN submission comments

- Package: igfetchr
- Version: 0.1.0
- First submission of igfetchr.

Platform testing performed locally by developer:
- macOS 12+ (local)
- Ubuntu 20.04 (local/container)
- Windows checks will be performed via win-builder on submission if requested.

Notes for CRAN maintainers:
- The package wraps the IG Trading REST API (labs.ig.com). Real API calls require a free API key from https://labs.ig.com and valid IG credentials.
- All examples and vignette use a built-in offline testing mode (environment variable `IGFETCHR_TESTING = "true"`) or are wrapped in `\dontrun{}` to avoid network access during checks. The vignette demonstrates usage with mock responses so it runs quickly.
- Unit tests are written to run without network access; tests set `IGFETCHR_TESTING = "true"` and use function `mock_response` parameters to supply deterministic results.
- No tests or examples perform live network access during R CMD check --as-cran.
- The package includes a clear risk disclaimer in DESCRIPTION, package-level docs, and function documentation: "Trading CFDs and spread bets carries a high risk of losing money. This package is for data access and is not financial advice."
- Suggested packages (testthat, httptest, lubridate) are in Suggests and not required at runtime.
- Note about future file timestamps: This is a system issue (unable to verify current time) and does not affect package functionality.

If you need any additional platform logs or further assurances about network-free checks, please let me know; I can provide reproducible logs from rhub or win-builder on request.
