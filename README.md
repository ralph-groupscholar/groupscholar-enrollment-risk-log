# GroupScholar Enrollment Risk Log

A Haskell CLI for logging enrollment risk signals, tracking who reported them, and generating quick summaries for operational triage.

## Features
- Log risk signals with consistent levels and categories
- List recent entries with timestamps and reporters
- Summarize risks by level and category for a given time window
- Postgres-backed storage with production-ready schema

## Tech
- Haskell (optparse-applicative, postgresql-simple)
- PostgreSQL

## Usage

Set environment variables for the production database:

```bash
export PGHOST=db-acupinir.groupscholar.com
export PGPORT=23947
export PGUSER=ralph
export PGPASSWORD='YOUR_PASSWORD'
export PGDATABASE=ralph
```

Run the CLI:

```bash
cabal run enrollment-risk-log -- add \
  --scholar-id GS-1042 \
  --scholar-name "Maya Patel" \
  --risk-level high \
  --category "financial" \
  --note "Missing tuition payment plan update" \
  --reported-by "ops@groupscholar.com"

cabal run enrollment-risk-log -- list --limit 10

cabal run enrollment-risk-log -- summary --days 30
```

## Database setup

Apply the schema and seed data in production:

```bash
psql "$PGDATABASE" -f db/schema.sql
psql "$PGDATABASE" -f db/seed.sql
```

## Testing

```bash
cabal test
```

## Project notes
- All tables live under the `groupscholar_enrollment_risk_log` schema.
- Keep risk levels to: `low`, `medium`, `high`, `critical`.
