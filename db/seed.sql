INSERT INTO groupscholar_enrollment_risk_log.risk_entries
  (scholar_id, scholar_name, risk_level, category, note, reported_by, reported_at, risk_score, status, owner, status_updated_at, resolved_at)
SELECT
  'GS-1042',
  'Maya Patel',
  'high',
  'financial',
  'Missing tuition payment plan update.',
  'ops@groupscholar.com',
  TIMESTAMPTZ '2026-02-05 14:20:00+00',
  75,
  'open',
  'ops-lead@groupscholar.com',
  TIMESTAMPTZ '2026-02-05 14:20:00+00',
  NULL
WHERE NOT EXISTS (
  SELECT 1 FROM groupscholar_enrollment_risk_log.risk_entries
  WHERE scholar_id = 'GS-1042'
    AND category = 'financial'
    AND reported_at::date = DATE '2026-02-05'
);

INSERT INTO groupscholar_enrollment_risk_log.risk_entries
  (scholar_id, scholar_name, risk_level, category, note, reported_by, reported_at, risk_score, status, owner, status_updated_at, resolved_at)
SELECT
  'GS-1091',
  'Jordan Lee',
  'medium',
  'academic',
  'GPA dropped below threshold last term.',
  'advisor@groupscholar.com',
  TIMESTAMPTZ '2026-01-31 16:05:00+00',
  50,
  'acknowledged',
  'academic-success@groupscholar.com',
  TIMESTAMPTZ '2026-02-01 09:00:00+00',
  NULL
WHERE NOT EXISTS (
  SELECT 1 FROM groupscholar_enrollment_risk_log.risk_entries
  WHERE scholar_id = 'GS-1091'
    AND category = 'academic'
    AND reported_at::date = DATE '2026-01-31'
);

INSERT INTO groupscholar_enrollment_risk_log.risk_entries
  (scholar_id, scholar_name, risk_level, category, note, reported_by, reported_at, risk_score, status, owner, status_updated_at, resolved_at)
SELECT
  'GS-1127',
  'Sofia Ramirez',
  'critical',
  'housing',
  'Housing insecurity reported; needs immediate support.',
  'support@groupscholar.com',
  TIMESTAMPTZ '2026-02-07 08:45:00+00',
  95,
  'open',
  'support-lead@groupscholar.com',
  TIMESTAMPTZ '2026-02-07 08:45:00+00',
  NULL
WHERE NOT EXISTS (
  SELECT 1 FROM groupscholar_enrollment_risk_log.risk_entries
  WHERE scholar_id = 'GS-1127'
    AND category = 'housing'
    AND reported_at::date = DATE '2026-02-07'
);

INSERT INTO groupscholar_enrollment_risk_log.risk_entries
  (scholar_id, scholar_name, risk_level, category, note, reported_by, reported_at, risk_score, status, owner, status_updated_at, resolved_at)
SELECT
  'GS-1178',
  'Amir Thompson',
  'low',
  'engagement',
  'Missed last cohort check-in but responded after follow-up.',
  'community@groupscholar.com',
  TIMESTAMPTZ '2026-01-25 11:15:00+00',
  25,
  'resolved',
  'community-team@groupscholar.com',
  TIMESTAMPTZ '2026-01-28 15:30:00+00',
  TIMESTAMPTZ '2026-01-28 15:30:00+00'
WHERE NOT EXISTS (
  SELECT 1 FROM groupscholar_enrollment_risk_log.risk_entries
  WHERE scholar_id = 'GS-1178'
    AND category = 'engagement'
    AND reported_at::date = DATE '2026-01-25'
);

INSERT INTO groupscholar_enrollment_risk_log.risk_entries
  (scholar_id, scholar_name, risk_level, category, note, reported_by, reported_at, risk_score, status, owner, status_updated_at, resolved_at)
SELECT
  'GS-1203',
  'Lina Chen',
  'high',
  'documentation',
  'Financial aid docs incomplete after reminder sequence.',
  'ops@groupscholar.com',
  TIMESTAMPTZ '2026-02-03 13:10:00+00',
  75,
  'acknowledged',
  'ops-specialist@groupscholar.com',
  TIMESTAMPTZ '2026-02-04 10:00:00+00',
  NULL
WHERE NOT EXISTS (
  SELECT 1 FROM groupscholar_enrollment_risk_log.risk_entries
  WHERE scholar_id = 'GS-1203'
    AND category = 'documentation'
    AND reported_at::date = DATE '2026-02-03'
);
