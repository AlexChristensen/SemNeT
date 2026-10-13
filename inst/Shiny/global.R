# Global setup ----
# Updated 13.10.2026

# Load {SemNeT}
suppressMessages(library(SemNeT))

# Maximum cores for app ----
# Updated 13.10.2026
max_app_cores <- function()
{

  # Return all cores but one
  max(1, parallel::detectCores() - 1)

}

# Set parallel plan for background tasks
future::plan(future::multisession, workers = max_app_cores())

# Load {shinyBS} for modal resources
suppressMessages(library(shinyBS))

# Route progress to Shiny
progressr::handlers(progressr::handler_shiny())

# Set maximum upload size
options(shiny.maxRequestSize = 50 * 1024^2)
