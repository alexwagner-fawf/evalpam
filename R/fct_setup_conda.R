#' Set up a conda environment for BirdNET inference
#'
#' Creates (or updates) a conda environment containing the Python packages
#' required by birdnetR. Use this when birdnetR's built-in managed virtual
#' environment cannot be used — for example on air-gapped machines, HPC
#' clusters, or when the system Python version is incompatible.
#'
#' birdnetR requires \code{numpy>=1.23.5,<2.0.0} and \code{birdnet==0.1.7}
#' with Python \code{>=3.9,<3.12}.  The defaults here satisfy all three
#' constraints.
#'
#' @param env_name Character. Name of the conda environment to create.
#'   Default \code{"birdnet-r"}.
#' @param conda Character. Path to the \code{conda} executable, or
#'   \code{"auto"} to let reticulate locate it automatically.
#'   Default \code{"auto"}.
#' @param python_version Character. Python version to install into the new
#'   environment.  Must satisfy \code{">=3.9,<3.12"}.  Default \code{"3.11"}.
#' @param force_reinstall Logical.  If \code{TRUE}, drop an existing
#'   environment with the same name and recreate it from scratch.
#'   Default \code{FALSE}.
#' @param https_proxy Character or \code{NULL}.  HTTPS proxy URL forwarded to
#'   pip when downloading packages (e.g. \code{"http://proxy.example:8080"}).
#'   \code{NULL} (default) keeps the current environment value.
#'
#' @return Invisibly, the file path to the Python executable inside the
#'   environment.  Pass this path to the \code{CONDA_ENV_NAME} parameter in
#'   \code{inst/02_inference.R} to activate the conda fallback.
#'
#' @seealso \code{inst/02_inference.R} for how to activate the conda fallback.
#' @export
setup_birdnet_conda <- function(env_name        = "birdnet-r",
                                conda           = "auto",
                                python_version  = "3.11",
                                force_reinstall = FALSE,
                                https_proxy     = NULL) {

  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required.")

  # ── Proxy setup ──────────────────────────────────────────────────────────────
  if (!is.null(https_proxy)) {
    old_proxy <- Sys.getenv("https_proxy", unset = NA)
    Sys.setenv(https_proxy = https_proxy, HTTPS_PROXY = https_proxy)
    on.exit({
      if (is.na(old_proxy)) Sys.unsetenv(c("https_proxy", "HTTPS_PROXY"))
      else Sys.setenv(https_proxy = old_proxy, HTTPS_PROXY = old_proxy)
    }, add = TRUE)
  }

  # ── Validate python_version against birdnetR constraint (>=3.9, <3.12) ──────
  pv <- numeric_version(python_version)
  if (pv < "3.9" || pv >= "3.12")
    stop("python_version '", python_version, "' must satisfy >=3.9,<3.12 (birdnetR requirement).")

  # ── Packages birdnetR 0.3.x requires (from its .onLoad py_require call) ─────
  packages <- c("numpy>=1.23.5,<2.0.0", "birdnet==0.1.7")

  # ── Check / remove existing environment ─────────────────────────────────────
  existing_envs <- tryCatch(
    reticulate::conda_list(conda = conda)$name,
    error = function(e) {
      stop("Could not list conda environments.  Is conda installed?\n  ", e$message)
    }
  )

  if (env_name %in% existing_envs) {
    if (force_reinstall) {
      message("Removing existing conda environment '", env_name, "'...")
      reticulate::conda_remove(envname = env_name, conda = conda)
    } else {
      message("Conda environment '", env_name, "' already exists.",
              "  Use force_reinstall = TRUE to recreate it.")
      py_path <- reticulate::conda_python(envname = env_name, conda = conda)
      message("  Python: ", py_path)
      return(invisible(py_path))
    }
  }

  # ── Create environment ───────────────────────────────────────────────────────
  message("Creating conda environment '", env_name,
          "' (Python ", python_version, ")...")
  reticulate::conda_create(
    envname        = env_name,
    python_version = python_version,
    conda          = conda
  )

  # ── Install pip packages ─────────────────────────────────────────────────────
  # birdnet is pip-only; pip = TRUE installs via pip inside the conda env.
  message("Installing: ", paste(packages, collapse = ", "), " ...")
  reticulate::py_install(
    packages = packages,
    envname  = env_name,
    method   = "conda",
    conda    = conda,
    pip      = TRUE
  )

  py_path <- reticulate::conda_python(envname = env_name, conda = conda)
  message("Setup complete.\n  Python: ", py_path,
          "\n  Pass this path (or the env name '", env_name,
          "') to CONDA_ENV_NAME in inst/02_inference.R.")
  invisible(py_path)
}
