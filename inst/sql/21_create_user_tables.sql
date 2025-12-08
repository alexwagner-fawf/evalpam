DROP TABLE IF EXISTS app_users;
CREATE TABLE app_users (
  user_id BIGSERIAL PRIMARY KEY,
  username VARCHAR(64) UNIQUE NOT NULL,
  password_hash TEXT NOT NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  first_name VARCHAR(50),
  last_name VARCHAR(50),
  email CITEXT UNIQUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  expire_date DATE DEFAULT NULL
);


DROP TABLE IF EXISTS app_user_roles;
CREATE TABLE app_user_roles (
  user_id BIGINT REFERENCES app_users(user_id) ON DELETE CASCADE,
  pg_role TEXT NOT NULL,
  PRIMARY KEY (user_id, pg_role)
);
