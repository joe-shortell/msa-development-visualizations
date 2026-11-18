library(rsconnect)
library(here)

### deploy the app to shinyapps.io ----------------------------------------------
# One command for a repeatable, non-interactive redeploy: `Rscript R/deploy.R`.
# Not sourced by 00_run_all.R; run it after 10 when you want the site updated.
#
# - appName is fixed here so every deploy updates the same app and URL
#   (https://j3shortell.shinyapps.io/msa-development-patterns/).
# - appFiles lists only what the running app needs, so tests/ and rsconnect/
#   are not bundled. .Renviron is included on purpose: it carries
#   CARTO_API_KEY (see app/R/constants.R).
# - forceUpdate skips the "update existing app?" prompt, lint skips the
#   pre-upload code scan, and launch.browser stops it opening a browser tab.
#
# Most deploy time is shinyapps.io installing packages (sf builds from source).
# It caches them between deploys, so redeploys are fast unless app.R's
# packages or their versions change.

app_dir <- here("app")
app_name <- "msa-development-patterns"

### files to bundle -------------------------------------------------------------
# Paths are relative to app/. Everything under R/, data/ and www/, plus the
# two top-level files.
app_files <- c(
  "app.R",
  ".Renviron",
  file.path("R", list.files(file.path(app_dir, "R"), recursive = TRUE)),
  file.path("data", list.files(file.path(app_dir, "data"), recursive = TRUE)),
  file.path("www", list.files(file.path(app_dir, "www"), recursive = TRUE))
)

# Fail before uploading if step 10's outputs or the key file are missing,
# rather than deploying an app that errors on launch.
required <- c("app.R", ".Renviron", file.path("data", "app_data.rds"))
missing <- required[!file.exists(file.path(app_dir, required))]
if (length(missing) > 0) {
  stop("Missing from app/: ", paste(missing, collapse = ", "),
       ". Run R/10_build_app_data.R (and add app/.Renviron) first.")
}

### deploy ----------------------------------------------------------------------
deployApp(
  appDir = app_dir,
  appFiles = app_files,
  appName = app_name,
  account = "j3shortell",
  server = "shinyapps.io",
  forceUpdate = TRUE,
  lint = FALSE,
  launch.browser = FALSE
)
