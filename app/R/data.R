### app data -------------------------------------------------------------------
# Everything the app reads is built by R/09_build_app_data.R (project root) into
# app/data/app_data.rds, so the app/ folder deploys on its own. The path is
# relative to app/, which is the working directory under shiny::runApp().

load_app_data <- function(path = file.path("data", "app_data.rds")) {
  if (!file.exists(path)) {
    stop(
      "App data not found at app/", path, ".\n",
      "  Build it from the project root with: Rscript R/09_build_app_data.R",
      call. = FALSE
    )
  }
  readRDS(path)
}
