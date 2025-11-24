REVOKE evalpam_birder, evalpam_admin FROM {`DB_EVALPAM_USER`};
REVOKE SELECT ON public.app_users FROM evalpam_user;
DROP USER IF EXISTS {`DB_EVALPAM_USER`};
DROP ROLE IF EXISTS evalpam_birder;
DROP ROLE IF EXISTS evalpam_admin;
CREATE ROLE evalpam_birder;
CREATE ROLE evalpam_admin;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO evalpam_birder;
GRANT SELECT, INSERT ON ALL TABLES IN SCHEMA public TO evalpam_birder;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO evalpam_admin;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA import TO evalpam_admin;

