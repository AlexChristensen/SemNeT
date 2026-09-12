# Sourced once by Shiny's own autoload mechanism, before ui.R/server.R --
# every file directly under inst/Shiny/R/ is also auto-sourced this way
# (Shiny's built-in convention for app-local helper/module files), so no
# manual `source()` loop is needed for the modules themselves.

suppressMessages(library(SemNeT))

# All cores the host has, minus one left free for the OS/main Shiny process.
# Defined here (not in mod_shared.R, where every other cross-module helper
# lives) because `global.R` is sourced before any `inst/Shiny/R/*.R` file
# (Shiny's own `loadSupport()` order) and needs this immediately, below, to
# size the app-wide `future` plan -- module files can still see it, since
# they're sourced into a child of this same environment
max_app_cores <- function()
{
  max(1, parallel::detectCores() - 1)
}

# Without an explicit plan here, `promises::future_promise()` (used by every
# `ExtendedTask` in the app) falls back to `future::sequential` -- meaning
# every "background" task actually runs synchronously in this same process,
# blocking Shiny's entire event loop (no UI updates, no responding to other
# input, not even serving a fresh page load) for the task's full duration.
# `multisession` dispatches each task to a real background R process instead.
# Sized off the same app-wide core cap used per-task so this doesn't
# provision more worker processes than the app already commits to
# elsewhere; `parallel_process()`'s own internal `plan()` switch (for a
# single task's *own* requested `cores`) is scoped to that task's worker via
# `on.exit()` and composes correctly with this outer plan
future::plan(future::multisession, workers = max_app_cores())

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
