DROP USER IF EXISTS evalpam_user;
CREATE USER evalpam_user WITH PASSWORD '4zB|cn{u&IvWF?YEy~0b%({-^Ct';
GRANT CONNECT ON DATABASE evalpam_db TO evalpam_user;
GRANT USAGE ON SCHEMA public TO evalpam_user;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO evalpam_user;

GRANT evalpam_birder, evalpam_admin TO evalpam_user;



