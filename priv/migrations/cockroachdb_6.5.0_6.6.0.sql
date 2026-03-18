CREATE TABLE blocklist (
    luser VARCHAR(250) NOT NULL,
    lserver VARCHAR(250) NOT NULL,
    reason TEXT,
    PRIMARY KEY (luser, lserver)
);
