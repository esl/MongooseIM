CREATE USER mongooseim WITH PASSWORD 'mongooseim_secret';
GRANT admin TO mongooseim;
GRANT ALL ON DATABASE mongooseim to mongooseim;
