CREATE USER  {`DB_EVALPAM_USER`} WITH PASSWORD {DB_PASSWORD_EVALPAM_USER};
GRANT CONNECT ON DATABASE evalpam_db TO evalpam_user;
GRANT evalpam_birder, evalpam_admin TO  {`DB_EVALPAM_USER`};
GRANT SELECT ON public.app_users TO evalpam_user;


