CREATE KEYSPACE IF NOT EXISTS mongooseim
        WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 1};

CREATE TABLE IF NOT EXISTS mongooseim.test_table (
    f1 int,
    f2 varchar,
    PRIMARY KEY (f1)
);