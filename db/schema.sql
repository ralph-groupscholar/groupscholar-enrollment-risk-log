CREATE SCHEMA IF NOT EXISTS groupscholar_enrollment_risk_log;

CREATE TABLE IF NOT EXISTS groupscholar_enrollment_risk_log.risk_entries (
  id BIGSERIAL PRIMARY KEY,
  scholar_id TEXT NOT NULL,
  scholar_name TEXT NOT NULL,
  risk_level TEXT NOT NULL CHECK (risk_level IN ('low','medium','high','critical')),
  category TEXT NOT NULL,
  note TEXT,
  reported_by TEXT NOT NULL,
  reported_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  risk_score INTEGER NOT NULL,
  status TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open','acknowledged','resolved')),
  owner TEXT,
  status_updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  resolved_at TIMESTAMPTZ
);

ALTER TABLE groupscholar_enrollment_risk_log.risk_entries
  ADD COLUMN IF NOT EXISTS status TEXT NOT NULL DEFAULT 'open'
    CHECK (status IN ('open','acknowledged','resolved'));

ALTER TABLE groupscholar_enrollment_risk_log.risk_entries
  ADD COLUMN IF NOT EXISTS owner TEXT;

ALTER TABLE groupscholar_enrollment_risk_log.risk_entries
  ADD COLUMN IF NOT EXISTS status_updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW();

ALTER TABLE groupscholar_enrollment_risk_log.risk_entries
  ADD COLUMN IF NOT EXISTS resolved_at TIMESTAMPTZ;

CREATE INDEX IF NOT EXISTS risk_entries_reported_at_idx
  ON groupscholar_enrollment_risk_log.risk_entries (reported_at DESC);

CREATE INDEX IF NOT EXISTS risk_entries_risk_level_idx
  ON groupscholar_enrollment_risk_log.risk_entries (risk_level);

CREATE INDEX IF NOT EXISTS risk_entries_status_idx
  ON groupscholar_enrollment_risk_log.risk_entries (status);

CREATE INDEX IF NOT EXISTS risk_entries_attention_idx
  ON groupscholar_enrollment_risk_log.risk_entries (status, risk_level, reported_at DESC);
