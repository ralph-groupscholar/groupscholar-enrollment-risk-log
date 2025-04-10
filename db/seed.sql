INSERT INTO groupscholar_enrollment_risk_log.risk_entries
  (scholar_id, scholar_name, risk_level, category, note, reported_by, reported_at, risk_score)
VALUES
  ('GS-1042', 'Maya Patel', 'high', 'financial', 'Missing tuition payment plan update.', 'ops@groupscholar.com', NOW() - INTERVAL '3 days', 75),
  ('GS-1091', 'Jordan Lee', 'medium', 'academic', 'GPA dropped below threshold last term.', 'advisor@groupscholar.com', NOW() - INTERVAL '8 days', 50),
  ('GS-1127', 'Sofia Ramirez', 'critical', 'housing', 'Housing insecurity reported; needs immediate support.', 'support@groupscholar.com', NOW() - INTERVAL '1 day', 95),
  ('GS-1178', 'Amir Thompson', 'low', 'engagement', 'Missed last cohort check-in but responded after follow-up.', 'community@groupscholar.com', NOW() - INTERVAL '14 days', 25),
  ('GS-1203', 'Lina Chen', 'high', 'documentation', 'Financial aid docs incomplete after reminder sequence.', 'ops@groupscholar.com', NOW() - INTERVAL '5 days', 75);
