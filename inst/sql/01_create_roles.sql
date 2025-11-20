
DROP ROLE IF EXISTS evalpam_birder;
DROP ROLE IF EXISTS evalpam_admin;
CREATE ROLE evalpam_birder;
CREATE ROLE evalpam_admin;
GRANT SELECT, INSERT ON ALL TABLES IN SCHEMA public TO evalpam_birder;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO evalpam_admin;

