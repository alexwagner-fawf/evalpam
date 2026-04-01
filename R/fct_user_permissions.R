#' #' Get User Role from Database
#' #'
#' #' @description Retrieves the role for a user after login
#' #'
#' #' @param pool Database connection pool
#' #' @param username Username from shinymanager login
#' #'
#' #' @return Character string with role name or NULL
#' #'
#' #' @noRd
#' get_user_role <- function(pool, username) {
#'   tryCatch({
#'     result <- DBI::dbGetQuery(
#'       pool,
#'       "SELECT pg_role FROM app_user_roles WHERE pameval_user = $1",
#'       params = list(username)
#'     )
#'
#'     if(nrow(result) == 0) {
#'       warning("No role found for user: ", username)
#'       return(NULL)
#'     }
#'
#'     return(result$pg_role[1])
#'
#'   }, error = function(e) {
#'     warning("Failed to get user role: ", e$message)
#'     return(NULL)
#'   })
#' }
#'
#'
#' #' Check if User has Permission
#' #'
#' #' @description Checks if user's role allows a specific action
#' #'
#' #' @param role User's role (from get_user_role)
#' #' @param action Action to check: "read", "write", "delete", "admin"
#' #'
#' #' @return Boolean
#' #'
#' #' @noRd
#' has_permission <- function(role, action) {
#'
#'   # Define permissions per role
#'   permissions <- list(
#'     evalpam_admin = c("read", "write", "delete", "admin", "manage_users"),
#'     birder = c("read", "write_own")  # can only write their own data
#'   )
#'
#'   if(is.null(role) || !role %in% names(permissions)) {
#'     return(FALSE)
#'   }
#'
#'   return(action %in% permissions[[role]])
#' }
#'
#'
#' #' Check if User can Access Data
#' #'
#' #' @description Checks if user can access specific data (e.g., edit only own records)
#' #'
#' #' @param username Current user
#' #' @param role Current user's role
#' #' @param data_owner Owner of the data record
#' #'
#' #' @return Boolean
#' #'
#' #' @noRd
#' can_access_data <- function(username, role, data_owner) {
#'
#'   # Admins can access everything
#'   if(has_permission(role, "admin")) {
#'     return(TRUE)
#'   }
#'
#'   # Birders can only access their own data
#'   if(role == "birder") {
#'     return(username == data_owner)
#'   }
#'
#'   return(FALSE)
#' }
#'
#'
#' #' Setup shinymanager with Database
#' #'
#' #' @description Configure shinymanager to use app_users table
#' #'
#' #' @param pool Database connection pool
#' #'
#' #' @return shinymanager credentials object
#' #'
#' #' @noRd
#' setup_shinymanager_credentials <- function(pool) {
#'
#'   # Fetch users from database
#'   users <- DBI::dbGetQuery(pool, "
#'     SELECT
#'       u.pameval_user as user,
#'       u.password,
#'       u.active as admin,  -- shinymanager uses 'admin' column
#'       r.pg_role
#'     FROM app_users u
#'     LEFT JOIN app_user_roles r ON u.pameval_user = r.pameval_user
#'     WHERE u.active = TRUE
#'   ")
#'
#'   # Create credentials object for shinymanager
#'   credentials <- data.frame(
#'     user = users$user,
#'     password = users$password,  # Already hashed with bcrypt
#'     admin = users$pg_role == "evalpam_admin",  # TRUE for admins
#'     stringsAsFactors = FALSE
#'   )
#'
#'   return(credentials)
#' }
#'
#'
#' #' UI Wrapper with shinymanager
#' #'
#' #' @description Wraps your app UI with shinymanager login
#' #'
#' #' @export
#' app_ui <- function(request) {
#'   tagList(
#'     # Your app resources
#'     golem_add_external_resources(),
#'
#'     # Wrap with shinymanager
#'     shinymanager::secure_app(
#'       ui = fluidPage(
#'         # Your actual UI here
#'         mod_main_ui("main")
#'       ),
#'       enable_admin = FALSE  # We handle admin ourselves
#'     )
#'   )
#' }
#'
#'
#' #' Server with Role-Based Access Control
#' #'
#' #' @export
#' app_server <- function(input, output, session) {
#'
#'   pool <- golem::get_golem_options("pool")
#'
#'   # shinymanager authentication
#'   res_auth <- shinymanager::secure_server(
#'     check_credentials = shinymanager::check_credentials(
#'       db = setup_shinymanager_credentials(pool),
#'       passphrase = get_golem_config("shinymanager_passphrase")
#'     )
#'   )
#'
#'   # Reactive values for user info
#'   user_info <- reactiveValues(
#'     username = NULL,
#'     role = NULL
#'   )
#'
#'   # Update user info after successful login
#'   observeEvent(res_auth$user, {
#'     user_info$username <- res_auth$user
#'     user_info$role <- get_user_role(pool, res_auth$user)
#'
#'     message("User logged in: ", user_info$username, " (Role: ", user_info$role, ")")
#'   })
#'
#'   # Pass user info to modules
#'   mod_main_server("main", pool = pool, user_info = user_info)
#' }
#'
#'
#' #' Example Module with RBAC
#' #'
#' #' @noRd
#' mod_main_server <- function(id, pool, user_info) {
#'   moduleServer(id, function(input, output, session) {
#'
#'     # Example: Show/hide UI based on role
#'     output$admin_panel <- renderUI({
#'       req(user_info$role)
#'
#'       if(has_permission(user_info$role, "admin")) {
#'         tagList(
#'           h3("Admin Panel"),
#'           actionButton(NS(id, "manage_users"), "Manage Users"),
#'           actionButton(NS(id, "view_logs"), "View Logs")
#'         )
#'       } else {
#'         NULL
#'       }
#'     })
#'
#'     # Example: Filter data based on role
#'     get_user_data <- reactive({
#'       req(user_info$username, user_info$role)
#'
#'       if(has_permission(user_info$role, "admin")) {
#'         # Admins see all data
#'         DBI::dbGetQuery(pool, "SELECT * FROM observations")
#'       } else {
#'         # Birders see only their own data
#'         DBI::dbGetQuery(
#'           pool,
#'           "SELECT * FROM observations WHERE created_by = $1",
#'           params = list(user_info$username)
#'         )
#'       }
#'     })
#'
#'     # Example: Delete with permission check
#'     observeEvent(input$delete_observation, {
#'       req(user_info$role)
#'
#'       if(!has_permission(user_info$role, "delete")) {
#'         showNotification("You don't have permission to delete", type = "error")
#'         return()
#'       }
#'
#'       # Get record owner
#'       record <- DBI::dbGetQuery(
#'         pool,
#'         "SELECT created_by FROM observations WHERE id = $1",
#'         params = list(input$selected_id)
#'       )
#'
#'       # Check if user can access this specific record
#'       if(!can_access_data(user_info$username, user_info$role, record$created_by[1])) {
#'         showNotification("You can only delete your own records", type = "error")
#'         return()
#'       }
#'
#'       # Perform deletion
#'       DBI::dbExecute(
#'         pool,
#'         "DELETE FROM observations WHERE id = $1",
#'         params = list(input$selected_id)
#'       )
#'
#'       showNotification("Deleted successfully", type = "message")
#'     })
#'   })
#' }
#'
#'
#' #' Helper: Add created_by column to tables
#' #'
#' #' @description SQL to track who created records
#' #'
#' #' @noRd
#' #'
#' #' Add this to your migration SQL:
#' #'
#' #' ALTER TABLE observations ADD COLUMN created_by VARCHAR(255);
#' #' ALTER TABLE observations ADD COLUMN created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP;
#' #' ALTER TABLE observations ADD FOREIGN KEY (created_by) REFERENCES app_users(pameval_user);
#' #'
#' #' -- Index for performance
#' #' CREATE INDEX idx_observations_created_by ON observations(created_by);
#' NULL
