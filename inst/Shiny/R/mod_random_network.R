# Random network module ----
# Updated 22.10.2026

# Random network interface ----
# Updated 13.10.2026
mod_random_network_ui <- function(id)
{

  # Set namespace
  ns <- NS(id)

  # Return interface
  tagList(

    # Add sidebar
    sidebarPanel(

      # Add iterations
      numericInput(ns("iter"), "Number of Iterations", value = 1000, min = 1, max = 20000, step = 100),

      # Add cores
      numericInput(ns("cores"), "Cores", value = 1, min = 1, max = max_app_cores()),

      # Add run button
      actionButton(ns("run"), label = "Perform Random Network Analyses", class = "btn-primary")

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
        "Erdős, P., & Rényi, A. (1960). On the evolution of random graphs. <em>Publications of the Mathematical ",
        "Institute of the Hungarian Academy of Sciences</em>, <em>5</em>, 17–61.<br>",
        "Maslov, S., & Sneppen, K. (2002). Specificity and stability in topology of protein networks. ",
        "<em>Science</em>, <em>296</em>, 910–913.<br><br>",
        "<em>p</em>-values are Monte Carlo <em>p</em>-values against Erdős–Rényi random networks (<code>p_value</code>) ",
        "and against degree-preserving random networks (<code>p_value_degree</code>); they are not adjusted for the ",
        "number of measures."
      )),
      style = "margin: 20px;"
    )

  )

}

# Random network server ----
# Updated 22.10.2026
mod_random_network_server <- function(id, state)
{

  # Start module
  moduleServer(id, function(input, output, session){

    # Set up progress file
    progress_file <- reactiveVal(NULL)

    # Poll progress
    progress <- progress_poller(session, progress_file)

    # Set up background task
    task <- ExtendedTask$new(function(networks, iter, cores, seed, progress_file){

      # Run in background
      promises::future_promise({

        # Set progress file
        options(SemNeT.progress_file = progress_file)

        # Perform random network tests
        do.call(random_network_test, c(networks, list(iter = iter, cores = cores, seed = seed)))

      }, seed = task_seed(seed))

    })

    # Watch for run button
    observeEvent(input$run, {

      # Require networks
      req(state$networks)

      # Warn about many iterations
      warn_if_slow(input$iter)

      # Create progress file
      progress_file(new_progress_file())

      # Start task
      task$invoke(state$networks, input$iter, input$cores, state$seed, progress_file())

    })

    # Watch for finished task
    observe({

      # Check for success
      if(task$status() == "success"){

        # Store result
        state$random_test <- task$result()

      }

    })

    # Render status
    output$status <- renderUI(progress_status_ui(task, progress))

    # Render results
    output$result <- renderTable(digits = 4, {

      # Require result
      req(state$random_test)

      # Combine results of groups
      do.call(rbind, lapply(names(state$random_test), function(name){

        # Add group and measure names
        cbind(Group = name, Measure = row.names(state$random_test[[name]]), state$random_test[[name]])

      }))

    })

  })

}
