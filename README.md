# evalpam

!!! IN CONSTRUCTION !!!

**`evalpam`** is a Shiny application for evaluating AI-annotated data from passive acoustic monitoring (PAM). The app is designed to streamline the assessment of automated annotations, helping researchers and practitioners ensure data quality and accuracy.

---

## Features

- Interactive Shiny app for exploring and evaluating AI-generated annotations
- Built with **[golem](https://thinkr-open.github.io/golem/)** for a robust, production-ready Shiny app
---

## Installation

Install the latest version of `evalpam` directly from GitHub:

```r
remotes::install_github("alexwagner-fawf/evalpam")
``` 

## Setting up the app for the first time

### Initializing the database 
A postgres server with admin rights (database creation) is required. Enter the credentials here. No admin credentials will be stored. Instead, a new postgres user will be defined (evalpam_username) with a given password (evalpam_pw). This password will be stored obfuscated in an .Renviron file at a defined location. The password is one neccessary information to access the app. 

```r
evalpam:::setup_app(user = "postgres",
                     host = "localhost",
                     port = 5432,
                     maintenance_dbname = "postgres",
                     password = "postgres",
                     evalpam_username = "evalpam_user",
                     evalpam_pw = "new_evalpam_pw",
                     evalpam_dbname = "evalpam_db",
                     admin_mailaddress = "emailaddress@required.com",
                     initialize_db = TRUE,
                     renviron_dir = NULL)
```

<img width="2004" height="1293" alt="Bildschirmfoto vom 2025-12-30 15-15-01" src="https://github.com/user-attachments/assets/d6217f06-199d-4105-a1e9-b0888105f522" />


# Adding users
After setting up the app and the database, you can add users to the database. The passwords will be stored in database tables encrypted with salt (bcrypt). 

Next, you setup a pool connection with admin rights for pameval_db. 

```r
pool <- evalpam:::set_db_pool(user = "postgres", password = "postgres")



evalpam:::add_users(pool, 
                      username = "birdfreak", 
                      password = "birdfreak", 
                      pg_role = "evalpam_birder", 
                      first_name = "Jon", 
                      last_name = "Doe", 
                      email = "jon.doe@web.com", 
                      active = TRUE, 
                      expire_date = Sys.Date()+7)

pw <- paste0(c(seq(9), letters)[floor(runif(10)*36)], collapse = "")

evalpam:::add_users(pool, 
                      username = "technerd", 
                      password = pw, 
                      pg_role = "evalpam_admin", 
                      first_name = "Jane", 
                      last_name = "Doe", 
                      email = "jane.doe@web.com", 
                      active = TRUE, 
                      expire_date = NA)

pool::poolClose(pool)

```

Now you created your first users. Admins will be allowed to add users and assess the whole dataset whereas birders will only have access to their own records. Now you can start the app and use the username and password credentials used in add_users . 

## Start the app
Once you successfully finished the previous steps, the app is ready to run. You have to enter the credentials of the add_users function to access it.

```r
evalpam::run_app()
```

## Two-phase verification workflow (screening then calibration)

The runnable scripts in `inst/` are numbered in execution order. Beyond ingest
(`01`), inference (`02`) and spectrogram generation (`03`), a two-phase
verification pipeline is supported:

- **Phase 1 — screening (with a stop criterion).** Generate `"top"`
  (highest-confidence) clips grouped by species × location, create occupancy
  groups with `groups_from_spectrograms(..., target_count = 2)`
  (`inst/04_assign_occupancy_groups.R`), and verify with the occupancy filter
  **on**. Each species × location is confirmed after 2 certain-present
  verifications, after which its remaining clips are skipped and a pop-up offers
  to jump to the next group.
- **Phase 2 — calibration (no stop criterion).** For the confirmed species,
  generate `"stratified"` clips (spread across the confidence range) and verify
  **all** of them with the occupancy filter **off** — e.g. to fit a logistic
  regression of manual presence on BirdNET score.

Every clip is tagged in `import.spectrograms.selection_mode`
(`top`/`random`/`stratified`/`custom`) so the two batches stay separable when
building the modelling table. The complete, commented walk-through — including
the SQL to list confirmed species and to assemble the calibration dataset — is
in **`inst/05_two_phase_occupancy_workflow.R`**.

> These occupancy/verification features require the database migrations
> `42`–`47` in `inst/sql/` to be applied by a privileged (table-owner) role.
