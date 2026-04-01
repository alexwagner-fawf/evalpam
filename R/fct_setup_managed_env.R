#' Check or repair birdnetR's managed virtual environment
#'
#' Verifies that birdnetR's built-in managed virtual environment (created and
#' controlled by reticulate) is functional.  Attempts to initialise it with the
#' same package constraints that birdnetR uses internally, then reports the
#' result.
#'
#' Call this interactively to diagnose why the inference script cannot load
#' birdnetR, before resorting to \code{\link{setup_birdnet_conda}}.
#'
#' @details
#' birdnetR's \code{.onLoad} hook calls
#' \code{Sys.setenv(RETICULATE_PYTHON = "managed")} and
#' \code{reticulate::py_require(c("numpy>=1.23.5,<2.0.0", "birdnet==0.1.7"),
#'   python_version = ">=3.9,<3.12")}.
#' This function replicates that sequence and forces Python initialisation so
#' any error surfaces here rather than inside a parallel worker.
#'
#' Common failure reasons and fixes:
#' \itemize{
#'   \item \strong{Python >=3.12 on PATH}: reticulate cannot satisfy the
#'     \code{<3.12} constraint.  Either install Python 3.9–3.11 system-wide or
#'     use \code{\link{setup_birdnet_conda}}.
#'   \item \strong{No network}: \code{birdnet==0.1.7} cannot be pip-installed.
#'     Pre-populate the managed venv on a connected machine and copy it, or use
#'     a pre-built conda env.
#'   \item \strong{Corrupt managed venv}: set \code{force_reinstall = TRUE} to
#'     wipe and recreate it.
#' }
#'
#' @param force_reinstall Logical.  If \code{TRUE}, delete the existing managed
#'   venv so reticulate recreates it from scratch.  Default \code{FALSE}.
#' @param https_proxy Character or \code{NULL}.  HTTPS proxy URL to use when
#'   pip downloads packages (e.g. \code{"http://proxy.example:8080"}).
#'   \code{NULL} (default) keeps whatever is already set in the environment.
#'   Pass an empty string \code{""} to explicitly unset the proxy.
#'
#' @return Invisibly, the path to the Python executable inside the managed venv,
#'   or \code{NULL} if initialisation fails.
#'
#' @seealso \code{\link{setup_birdnet_conda}} for the conda fallback.
#' @export
check_birdnet_managed_env <- function(force_reinstall = FALSE,
                                      https_proxy     = NULL) {

  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required.")

  # ── Proxy setup ──────────────────────────────────────────────────────────────
  # pip (called by py_require) respects https_proxy / HTTPS_PROXY env vars.
  # If neither is set and a proxy is needed, pip cannot reach PyPI.
  if (!is.null(https_proxy)) {
    old_proxy <- Sys.getenv("https_proxy", unset = NA)
    Sys.setenv(https_proxy = https_proxy, HTTPS_PROXY = https_proxy)
    on.exit({
      if (is.na(old_proxy)) Sys.unsetenv(c("https_proxy", "HTTPS_PROXY"))
      else Sys.setenv(https_proxy = old_proxy, HTTPS_PROXY = old_proxy)
    }, add = TRUE)
  }

  current_proxy <- Sys.getenv("https_proxy", unset = Sys.getenv("HTTPS_PROXY", unset = ""))

  # ── Optionally wipe the existing managed venv ────────────────────────────────
  if (force_reinstall) {
    venv_name <- "r-reticulate"
    existing  <- tryCatch(reticulate::virtualenv_list(), error = function(e) character(0))
    if (venv_name %in% existing) {
      message("Removing managed venv '", venv_name, "'...")
      reticulate::virtualenv_remove(venv_name, confirm = FALSE)
    } else {
      message("Managed venv '", venv_name, "' not found; nothing to remove.")
    }
  }

  # ── Replicate birdnetR .onLoad — fail fast so errors are visible ─────────────
  message(
    "Attempting to initialise birdnetR managed virtual environment...",
    if (nchar(current_proxy) > 0) paste0("\n  Proxy: ", current_proxy)
    else "\n  No proxy set (set https_proxy if pip cannot reach PyPI)"
  )

  Sys.setenv(RETICULATE_PYTHON = "managed")

  .is_network_error <- function(msg) {
    grepl("fetch|connect|dns|tunnel|network|pypi\\.org|lookup",
          msg, ignore.case = TRUE)
  }

  pkg_result <- tryCatch({
    reticulate::py_require(
      packages       = c("numpy>=1.23.5,<2.0.0", "birdnet==0.1.7"),
      python_version = ">=3.9,<3.12",
      action         = "add"
    )
    TRUE
  }, error = function(e) {
    msg <- conditionMessage(e)
    message("  py_require failed: ", msg)
    if (.is_network_error(msg) && nchar(current_proxy) == 0) {
      message(
        "\n  Looks like a network/proxy error.",
        "\n  If you are behind a proxy, retry with:",
        "\n    check_birdnet_managed_env(https_proxy = \"http://<host>:<port>\")",
        "\n  Or set it persistently in .Renviron:",
        "\n    https_proxy=http://<host>:<port>"
      )
    }
    FALSE
  })

  if (!pkg_result) {
    message(
      "\nManaged env setup failed.  Other options:\n",
      "  1. Install Python 3.9–3.11 and ensure it is on PATH.\n",
      "  2. Use the conda fallback:\n",
      "       setup_birdnet_conda()  # creates env 'birdnet-r'\n",
      "     Then set CONDA_ENV_NAME <- \"birdnet-r\" in inst/02_inference.R."
    )
    return(invisible(NULL))
  }

  py_path <- tryCatch({
    reticulate::py_config()
    reticulate::py_exe()
  }, error = function(e) {
    message("  py_config() failed: ", conditionMessage(e))
    NULL
  })

  if (is.null(py_path)) {
    message("Python initialisation failed after py_require succeeded.",
            "\n  Try check_birdnet_managed_env(force_reinstall = TRUE).")
    return(invisible(NULL))
  }

  pv <- tryCatch(
    numeric_version(reticulate::py_version()),
    error = function(e) numeric_version("0")
  )

  message(
    "Managed env OK.\n",
    "  Python    : ", py_path, "\n",
    "  Version   : ", pv, "\n",
    "  Constraint: >=3.9,<3.12 — ",
    if (pv >= "3.9" && pv < "3.12") "SATISFIED" else "NOT SATISFIED (unexpected)"
  )

  invisible(py_path)
}
