# Sourced once by Shiny's own autoload mechanism, before ui.R/server.R --
# every file directly under inst/Shiny/R/ is also auto-sourced this way
# (Shiny's built-in convention for app-local helper/module files), so no
# manual `source()` loop is needed for the modules themselves.

suppressMessages(library(SemNeT))

# `shinyBS` only registers its CSS/JS resource path (`addResourcePath("sbs",
# ...)`) from its own `.onAttach` hook -- that only fires via `library()`,
# not via `shinyBS::` namespace access, which is all any module uses. Without
# this, `bsModal()` still builds correctly but its JS/CSS 404s and the modal
# never opens
suppressMessages(library(shinyBS))

# `parallel_process()` (used by every long-running analysis) already reports
# progress via {progressr} -- routing it to Shiny's own progress UI is a
# one-line hookup, not something the app has to build itself
progressr::handlers(progressr::handler_shiny())

# Shiny's default upload cap is a silent 5MB; make it explicit and generous
# enough for realistic fluency datasets
options(shiny.maxRequestSize = 50 * 1024^2)
