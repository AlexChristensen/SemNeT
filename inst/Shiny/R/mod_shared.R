# Small helpers shared across modules -- app/UI glue only, no SemNeT
# analysis logic lives here.

# Hard ceiling on the cores any single session can request, independent of
# the host machine's actual core count -- important on a shared, hosted
# deployment so one session can't claim most of a shared host
max_app_cores <- function()
{
  max(1, min(parallel::detectCores() - 1, 8))
}

# A confirmation-style warning above a large iteration count, matching the
# spirit of the original app's data-size warning but applied to iterations
warn_if_slow <- function(iter, threshold = 2000)
{
  if(iter > threshold){
    shinyalert::shinyalert(
      title = paste0(iter, " iterations requested"),
      text = "This may take a while and use significant memory. The tab will update when it's done.",
      type = "warning"
    )
  }
}

# Status/error UI for an `ExtendedTask`, shared across every module that
# runs a long analysis through one -- without this, an error inside a
# background task (bad data, a degenerate network, etc.) leaves the tab
# stuck showing "Running..." forever with no feedback, since `task$result()`
# re-throws the error wherever it's read
task_status_ui <- function(task)
{
  status <- task$status()
  if(status == "running"){
    tags$p(tags$em("Running... this tab will update when it's done."))
  }else if(status == "error"){
    message <- tryCatch(task$result(), error = function(e) conditionMessage(e))
    tags$p(style = "color: #a94442;", paste("Failed:", message))
  }
}

# One spreading-activation time step's network plot, via `qgraph::qgraph()`
# directly (no SemNeT function does this -- it's genuinely presentation-only
# glue, same category as `read_uploaded_file()`). Computed live from `sa`
# each time the slider moves, rather than precomputing and retaining a
# recorded plot per time step up front -- the same "don't keep what isn't
# needed yet" principle applied everywhere else in this rebuild
plot_spreading_frame <- function(network, sa, time_step)
{

  frame <- sa[sa$time == time_step, ]
  activation <- stats::setNames(frame$activation, frame$node)[colnames(network)]
  activation[is.na(activation)] <- 0

  # Node fill color intensity reflects activation (white = none, orange = max
  # activation observed across the whole simulation, not just this frame, so
  # color is comparable across time steps)
  max_activation <- max(sa$activation, 1e-8)
  intensity <- pmin(activation / max_activation, 1)
  colors <- grDevices::rgb(1, 1 - intensity, 1 - intensity)

  qgraph::qgraph(
    network, color = colors, labels = colnames(network),
    title = paste0("Time step ", time_step)
  )

}
