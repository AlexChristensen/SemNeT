# Random walk module ----
# Updated 22.10.2026

# Random walk interface ----
# Updated 13.10.2026
mod_random_walk_ui <- function(id)
{

  # Set namespace
  ns <- NS(id)

  # Return interface
  tagList(

    # Add sidebar
    sidebarPanel(

      # Add network selectors
      uiOutput(ns("network_selectors")),

      # Add starting steps
      numericInput(ns("steps"), "Starting Number of Steps", value = 10, min = 1, step = 1),

      # Add maximum steps
      numericInput(ns("reps"), "Maximum Steps / 10 (steps run from the start to 10 x this, in increments of 10)", value = 20, min = 1, max = 50, step = 5),

      # Add iterations
      numericInput(ns("iter"), "Number of Iterations", value = 10000, min = 1, max = 50000, step = 1000),

      # Add cores
      numericInput(ns("cores"), "Cores", value = 1, min = 1, max = max_app_cores()),

      # Add run button
      actionButton(ns("run"), label = "Perform Random Walk Analyses", class = "btn-primary")

    ),

    # Add main panel
    mainPanel(

      # Add status
      uiOutput(ns("status")),

      # Add results table
      tableOutput(ns("result"))

    ),

    # Add citation footer
    tags$footer(
      HTML(paste0(
        "<b>Please cite:</b><br>",
        "Kenett, Y. N., & Austerweil, J. L. (2016). Examining search processes in low and high creative ",
        "individuals with random walks. In <em>Proceedings of the 38th annual meeting of the cognitive ",
        "science society</em> (pp. 313–318). Austin, TX."
      )),
      style = "margin: 20px;"
    )

  )

}

# Random walk server ----
# Updated 22.10.2026
mod_random_walk_server <- function(id, state)
{

  # Start module
  moduleServer(id, function(input, output, session){

    # Render network selectors
    output$network_selectors <- renderUI({

      # Require networks
      req(state$networks)

      # Get namespace
      ns <- session$ns

      # Get network names
      names <- names(state$networks)

      # Show selectors
      tagList(
        selectInput(ns("network_a"), "Select Network A", choices = names, selected = names[1]),
        selectInput(ns("network_b"), "Select Network B", choices = names, selected = names[min(2, length(names))])
      )

    })

    # Set up progress file
    progress_file <- reactiveVal(NULL)

    # Poll progress
    progress <- progress_poller(session, progress_file)

    # Set up background task
    task <- ExtendedTask$new(function(network_a, network_b, name_a, name_b, reps, steps, iter, cores, seed, progress_file){

      # Run in background
      promises::future_promise({

        # Set progress file
        options(SemNeT.progress_file = progress_file)

        # Perform random walk
        random_walk(network_a, network_b, name_a = name_a, name_b = name_b, reps = reps, steps = steps, iter = iter, cores = cores)

      }, seed = task_seed(seed))

    })

    # Watch for run button
    observeEvent(input$run, {

      # Require networks
      req(state$networks, input$network_a, input$network_b)

      # Check for same networks
      if(input$network_a == input$network_b){

        # Send warning alert
        shinyalert::shinyalert(title = "Pick two different networks", type = "warning")

        # Return nothing
        return(invisible(NULL))

      }

      # Warn about many iterations
      warn_if_slow(input$iter * input$reps)

      # Create progress file
      progress_file(new_progress_file())

      # Start task
      task$invoke(
        state$networks[[input$network_a]], state$networks[[input$network_b]],
        input$network_a, input$network_b, input$reps, input$steps, input$iter, input$cores, state$seed,
        progress_file()
      )

    })

    # Watch for finished task
    observe({

      # Check for success
      if(task$status() == "success"){

        # Store result
        state$random_walk <- task$result()

      }

    })

    # Render status
    output$status <- renderUI(progress_status_ui(task, progress))

    # Render results
    output$result <- renderTable({

      # Require result
      req(state$random_walk)

      # Get short results
      shown <- state$random_walk$short

      # Convert steps to integers
      shown$Steps <- as.integer(shown$Steps)

      # Return results
      shown

    }, rownames = FALSE, digits = 3, caption = paste(
      "Random Walk Results: differences are the first network minus the second; d is Cohen's d of the simulated walks;",
      "\"negligible\" means |d| < .2. These describe simulated walks over the estimated networks and do not account",
      "for uncertainty in the networks (see the Bootstrap tab)."
    ), caption.placement = "top")

  })

}
