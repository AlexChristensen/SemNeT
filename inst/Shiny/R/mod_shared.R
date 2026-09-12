# Small helpers shared across modules -- app/UI glue only, no SemNeT
# analysis logic lives here. `max_app_cores()` lives in global.R instead (it
# needs to exist before this file is even sourced -- see global.R's comment).

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

# Status/error UI for an `ExtendedTask` with no meaningful per-iteration
# progress to show (e.g. Spreading Activation's single `spreadr::spreadr()`
# call) -- without this, an error inside a background task (bad data, a
# degenerate network, etc.) leaves the tab stuck showing "Running..." forever
# with no feedback, since `task$result()` re-throws the error wherever it's read
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

# A fresh per-invocation progress file pair: `file` holds the current total
# (pre-populated with "1" so a poller reading it before the task's first real
# write sees a valid, if uninformative, value rather than an empty file), and
# the sibling "<file>.progress" is appended to once per completed iteration
# by `parallel_process()` (see helpers.R for why a completion *count* there,
# rather than the raw iteration index here, is what stays monotonic under
# `cores > 1`)
new_progress_file <- function()
{
  file <- tempfile()
  writeLines("1", file)
  file.create(paste0(file, ".progress"))
  return(file)
}

# Live per-iteration progress for a long-running `ExtendedTask`. `progressr`'s
# own cross-process relay (`handlers(handler_shiny())`, set in global.R) only
# reaches a caller that synchronously blocks waiting on the future (e.g. a
# plain interactive `future::value()` call) -- confirmed empirically that it
# delivers nothing at all to a `shiny::ExtendedTask`/`promises::future_promise()`
# caller, which never blocks that way. Instead, the task itself is invoked
# with `options(SemNeT.progress_file = <this file>)` set *inside* its own
# `future_promise()` body (see e.g. mod_bootstrap.R), so `parallel_process()`
# (in helpers.R) writes "<current> <total>" there directly from the
# background worker -- a plain file being the simplest channel that needs no
# cross-process relay at all, just ordinary filesystem access from both sides.
# `file_reactive` is a `reactiveVal()` holding the current task's file path
# (`NULL` before any task has been invoked yet)
progress_poller <- function(session, file_reactive)
{

  shiny::reactivePoll(
    400, session,
    checkFunc = function(){
      file <- file_reactive()
      if(is.null(file) || !file.exists(file)){
        return(NULL)
      }
      progress_file <- paste0(file, ".progress")
      # File size is a monotonically increasing signal of completions so far
      # (each one appends exactly one byte) -- unlike mtime, it's also usable
      # directly as the progress count itself in `valueFunc()` below
      if(file.exists(progress_file)) file.info(progress_file)$size else NA
    },
    valueFunc = function(){

      file <- file_reactive()

      if(is.null(file) || !file.exists(file)){
        return(NULL)
      }

      total <- suppressWarnings(as.numeric(readLines(file, n = 1, warn = FALSE)))

      if(length(total) != 1 || anyNA(total)){
        return(NULL)
      }

      progress_file <- paste0(file, ".progress")
      current <- if(file.exists(progress_file)) file.info(progress_file)$size else 0

      return(list(current = current, total = total))

    }
  )

}

# A real progress bar while `task` is running (falling back to a plain
# "Running..." message before the first real progress value has been
# written yet), or the same error display as `task_status_ui()` above.
# `progress` is a reactive as returned by `progress_poller()`
progress_status_ui <- function(task, progress)
{

  status <- task$status()

  if(status == "error"){
    message <- tryCatch(task$result(), error = function(e) conditionMessage(e))
    return(tags$p(style = "color: #a94442;", paste("Failed:", message)))
  }

  if(status != "running"){
    return(NULL)
  }

  info <- progress()

  if(is.null(info) || info$total <= 0){
    return(tags$p(tags$em("Running... this tab will update when it's done.")))
  }

  percent <- round(100 * min(info$current / info$total, 1))

  tags$div(
    style = "width: 100%; max-width: 400px; background: #e9ecef; border-radius: 4px; overflow: hidden; margin-bottom: 10px;",
    tags$div(
      style = paste0(
        "width: ", percent, "%; background: #337ab7; color: white; text-align: center; ",
        "padding: 4px 0; white-space: nowrap; transition: width 0.3s ease;"
      ),
      paste0(percent, "% (", info$current, " / ", info$total, ")")
    )
  )

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
