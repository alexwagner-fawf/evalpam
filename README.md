# evalpam

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

# Adding users
After setting up the app and the database, you can add users to the database. The passwords will be stored in database tables encrypted with salt (bcrypt). 

Next, you setup a pool connection with admin rights for pameval_db. 

```r
pool <- evalpam:::set_db_pool(user = "postgres", password = "postgres")
evalpam:::add_users(pool, pameval_user = "birdfreak", password = "password", pg_role = "evalpam_birder", active = TRUE)
```

Now you created your first user and you can start the app

## Start the app
Once you successfully finished the previous steps, the app is ready to run. You have to enter the credentials of the add_users function to access it.

```r
evalpam::run_app()
``` 
