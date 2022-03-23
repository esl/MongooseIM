-- MOD_INBOX
ALTER TABLE inbox ALTER COLUMN archive DROP DEFAULT;
ALTER TABLE inbox CHANGE archive box VARCHAR(64) NOT NULL;
UPDATE inbox SET box = 'inbox' WHERE box = '0';
UPDATE inbox SET box = 'archive' WHERE box = '1';
ALTER TABLE inbox ALTER COLUMN box SET DEFAULT 'inbox';
