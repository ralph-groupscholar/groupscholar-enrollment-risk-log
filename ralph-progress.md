# Ralph Progress Log

## 2026-02-08
- Initialized `groupscholar-enrollment-risk-log` Haskell CLI with Postgres schema and seed data.
- Added risk entry commands, summary reporting, and Hspec tests.

## 2026-02-08
- Added status/owner tracking with update and attention commands for follow-up workflows.
- Expanded schema/seed data to include status fields and backfilled production tables.
- Updated summaries and tests to cover risk status parsing.

## 2026-02-08
- Added queue command plus status-filtered listing with entry IDs and improved attention ordering.
- Expanded documentation with queue usage examples and tightened CLI output for triage workflows.
- Attempted tests; cabal not available in the environment.

## 2026-02-08
- Added aging command to flag unresolved risks older than a threshold with age buckets.
- Introduced RiskAging module with bucket logic and tests, plus README updates.
