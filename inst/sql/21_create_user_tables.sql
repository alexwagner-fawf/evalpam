DROP TABLE IF EXISTS app_users;
CREATE TABLE app_users (
  pameval_user TEXT PRIMARY KEY,
  password_hash TEXT NOT NULL,
  active BOOLEAN DEFAULT TRUE,
  created_at TIMESTAMP
);

DROP TABLE IF EXISTS app_user_roles;
CREATE TABLE app_user_roles (
  pameval_user TEXT PRIMARY KEY,
  pg_role TEXT NOT NULL
);
