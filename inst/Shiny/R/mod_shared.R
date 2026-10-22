# Shared helpers ----
# Updated 22.10.2026

# Internal function of {SemNeT} ----
# Updated 22.10.2026
semnet_internal <- function(name)
{

  # Return function from namespace
  return(utils::getFromNamespace(name, "SemNeT"))

}

# Text lines as they print in the console ----
# Updated 22.10.2026
console_lines <- function(lines)
{

  # Check for no lines
  if(length(lines) == 0){

    # Return nothing
    return(NULL)

  }

  # Return lines with spacing kept
  return(tags$div(style = "white-space: pre-wrap; margin-bottom: 5px;", paste(lines, collapse = "\n")))

}

# Table as it prints in the console ----
# Updated 22.10.2026
console_table <- function(table)
{

  # Return table (values are already formatted for the console)
  return(tags$table(
    class = "table table-condensed table-striped", style = "width: auto;",
    tags$thead(tags$tr(lapply(names(table), function(name) tags$th(style = "text-align: right;", name)))),
    tags$tbody(lapply(seq_len(nrow(table)), function(i){
      tags$tr(lapply(table[i, ], function(value) tags$td(style = "text-align: right;", as.character(value))))
    }))
  ))

}

# Display of test as it prints in the console ----
# Updated 22.10.2026
console_display_ui <- function(display)
{

  # Get sections (permutation tests have one)
  sections <- if(is.null(display$sections)){

    # Set single section
    list(list(
      settings = display$settings, note = NULL, comparison = display$comparison,
      table = display$table, footnote = NULL
    ))

  }else{

    # Use sections
    display$sections

  }

  # Return display
  return(tagList(
    tags$h4(display$header),
    lapply(sections, function(section){
      tagList(
        console_lines(c(section$settings, section$note, section$comparison)),
        console_table(section$table),
        console_lines(section$footnote)
      )
    }),
    lapply(display$notes, function(note) tags$p(tags$em(note))),
    lapply(display$notices, function(note) div(class = "alert alert-warning", note))
  ))

}

# Validate seed ----
# Updated 22.10.2026
valid_seed <- function(seed)
{

  # Check for whole number within range
  return(
    length(seed) == 1 && is.numeric(seed) && is.finite(seed) &&
      seed == floor(seed) && seed >= 0 && seed <= .Machine$integer.max
  )

}

# Seed for analyses ----
# Updated 22.10.2026
analysis_seed <- function(seed)
{

  # Return seed for {SemNeT} functions (NULL is not reproducible)
  return(if(valid_seed(seed) && seed != 0) seed else NULL)

}

# Seed for background tasks ----
# Updated 22.10.2026
task_seed <- function(seed)
{

  # Return seed for {promises} (TRUE draws a random seed)
  return(if(is.null(seed)) TRUE else as.integer(seed))

}

# Warn about slow analyses ----
# Updated 13.10.2026
warn_if_slow <- function(iter, threshold = 2000)
{

  # Check for many iterations
  if(iter > threshold){

    # Send alert
    shinyalert::shinyalert(
      title = paste0(iter, " iterations requested"),
      text = "This may take a while and use significant memory. The tab will update when it's done.",
      type = "warning"
    )

  }

}

# Status of task ----
# Updated 13.10.2026
task_status_ui <- function(task)
{

  # Get status of task
  status <- task$status()

  # Check for running task
  if(status == "running"){

    # Show running message
    tags$p(tags$em("Running... this tab will update when it's done."))

  }else if(status == "error"){

    # Get error message
    message <- tryCatch(task$result(), error = function(e) conditionMessage(e))

    # Show error message
    tags$p(style = "color: #a94442;", paste("Failed:", message))

  }

}

# New progress file ----
# Updated 13.10.2026
new_progress_file <- function()
{

  # Create file for total
  file <- tempfile()

  # Write placeholder total
  writeLines("1", file)

  # Create file for completions
  file.create(paste0(file, ".progress"))

  # Return file
  return(file)

}

# Poll progress ----
# Updated 13.10.2026
progress_poller <- function(session, file_reactive)
{

  # Poll progress files
  shiny::reactivePoll(
    400, session,
    checkFunc = function(){

      # Get progress file
      file <- file_reactive()

      # Check for no file
      if(is.null(file) || !file.exists(file)){

        # Return nothing
        return(NULL)

      }

      # Get completion file
      progress_file <- paste0(file, ".progress")

      # Return size of completion file
      if(file.exists(progress_file)) file.info(progress_file)$size else NA

    },
    valueFunc = function(){

      # Get progress file
      file <- file_reactive()

      # Check for no file
      if(is.null(file) || !file.exists(file)){

        # Return nothing
        return(NULL)

      }

      # Get total iterations
      total <- suppressWarnings(as.numeric(readLines(file, n = 1, warn = FALSE)))

      # Check for invalid total
      if(length(total) != 1 || anyNA(total)){

        # Return nothing
        return(NULL)

      }

      # Get completion file
      progress_file <- paste0(file, ".progress")

      # Get completed iterations
      current <- if(file.exists(progress_file)) file.info(progress_file)$size else 0

      # Return progress
      return(list(current = current, total = total))

    }
  )

}

# Status with progress bar ----
# Updated 13.10.2026
progress_status_ui <- function(task, progress)
{

  # Get status of task
  status <- task$status()

  # Check for error
  if(status == "error"){

    # Get error message
    message <- tryCatch(task$result(), error = function(e) conditionMessage(e))

    # Show error message
    return(tags$p(style = "color: #a94442;", paste("Failed:", message)))

  }

  # Check for task not running
  if(status != "running"){

    # Show nothing
    return(NULL)

  }

  # Get progress
  info <- progress()

  # Check for no progress
  if(is.null(info) || info$total <= 0){

    # Show running message
    return(tags$p(tags$em("Running... this tab will update when it's done.")))

  }

  # Get percent complete
  percent <- round(100 * min(info$current / info$total, 1))

  # Show progress bar
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

# Plot spreading activation frame ----
# Updated 13.10.2026
plot_spreading_frame <- function(network, sa, time_step)
{

  # Get activation of time step
  frame <- sa[sa$time == time_step, ]

  # Get activation of each node
  activation <- stats::setNames(frame$activation, frame$node)[colnames(network)]

  # Set missing activation to zero
  activation[is.na(activation)] <- 0

  # Get maximum activation
  max_activation <- max(sa$activation, 1e-8)

  # Get relative activation
  intensity <- pmin(activation / max_activation, 1)

  # Get node colors
  colors <- grDevices::rgb(1, 1 - intensity, 1 - intensity)

  # Plot network
  qgraph::qgraph(
    network, color = colors, labels = colnames(network),
    title = paste0("Time step ", time_step)
  )

}
