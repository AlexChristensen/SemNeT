# Permutation module ----
# Updated 22.10.2026

# Permutation interface ----
# Updated 22.10.2026
mod_permutation_ui <- function(id)
{

  # Set namespace
  ns <- NS(id)

  # Return interface
  tagList(

    # Add sidebar
    sidebarPanel(

      # Add sample selectors
      uiOutput(ns("group_selectors")),

      # Add measure selection
      checkboxGroupInput(
        ns("measure"), "Network Measures",
        choiceNames = c("ASPL", "CC", "Q"), choiceValues = c("aspl", "cc", "q"),
        selected = c("aspl", "cc", "q"), inline = TRUE
      ),

      # Add alternative selection
      selectInput(
        ns("alternative"), "Alternative Hypothesis",
        choices = c(
          "Two-tailed" = "two_tailed", "Two-tailed (from null center)" = "two_tailed_centered",
          "Two-tailed (twice smaller one-sided)" = "two_tailed_doubled", "Less" = "less", "Greater" = "greater"
        )
      ),

      # Add iterations
      numericInput(ns("iter"), "Number of Iterations", value = 1000, min = 1, max = 20000, step = 100),

      # Add cores
      numericInput(ns("cores"), "Cores", value = 1, min = 1, max = max_app_cores()),

      # Add run button
      actionButton(ns("run"), label = "Perform Permutation Analysis", class = "btn-primary"),

      # Add reuse button
      actionButton(ns("run_again"), label = "Test Another Measure (reuses permuted networks)"),

      # Add spacing
      br(), br(),

      # Add plot button
      actionButton(ns("run_plot"), label = "Generate Permutation Plot")

    ),

    # Add main panel
    mainPanel(

      # Add status
      uiOutput(ns("status")),

      # Add results table (as printed in the console)
      uiOutput(ns("result")),

      # Add plot
      plotOutput(ns("perm_plot"))

    )

  )

}

# Permutation server ----
# Updated 22.10.2026
mod_permutation_server <- function(id, state)
{

  # Start module
  moduleServer(id, function(input, output, session){

    # Render sample selectors
    output$group_selectors <- renderUI({

      # Require groups
      req(state$groups_data)

      # Get namespace
      ns <- session$ns

      # Get group names
      names <- names(state$groups_data)

      # Show selectors
      tagList(
        selectInput(ns("sample_1"), "Sample 1", choices = names, selected = names[1]),
        selectInput(ns("sample_2"), "Sample 2", choices = names, selected = names[min(2, length(names))])
      )

    })

    # Set up progress file
    progress_file <- reactiveVal(NULL)

    # Poll progress
    progress <- progress_poller(session, progress_file)

    # Set up background task
    task <- ExtendedTask$new(function(sample_1, sample_2, method, method_args, sim, minimum_cases, measure, alternative, iter, cores, seed, progress_file){

      # Run in background
      promises::future_promise({

        # Set progress file
        options(SemNeT.progress_file = progress_file)

        # Perform permutation test
        permutation_SemNeT(
          sample_1 = sample_1, sample_2 = sample_2, method = method, method_args = method_args,
          sim = sim, minimum_cases = minimum_cases, measure = measure, alternative = alternative,
          iter = iter, cores = cores, keep_networks = TRUE, seed = seed
        )

      }, seed = task_seed(seed))

    })

    # Set up background task for reusing networks
    reuse_task <- ExtendedTask$new(function(previous, measure, alternative, cores, progress_file){

      # Run in background
      promises::future_promise({

        # Set progress file
        options(SemNeT.progress_file = progress_file)

        # Perform permutation test with previous networks
        permutation_SemNeT(previous = previous, measure = measure, alternative = alternative, cores = cores)

      }, seed = TRUE)

    })

    # Set up names of samples
    sample_names <- reactiveVal(NULL)

    # Watch for run button
    observeEvent(input$run, {

      # Require groups, method, and samples
      req(state$groups_data, state$method, input$sample_1, input$sample_2, input$measure)

      # Check for same samples
      if(input$sample_1 == input$sample_2){

        # Send warning alert
        shinyalert::shinyalert(title = "Pick two different samples", type = "warning")

        # Return nothing
        return(invisible(NULL))

      }

      # Warn about many iterations
      warn_if_slow(input$iter)

      # Store names of samples
      sample_names(c(input$sample_1, input$sample_2))

      # Create progress file
      progress_file(new_progress_file())

      # Start task
      task$invoke(
        state$groups_data[[input$sample_1]], state$groups_data[[input$sample_2]],
        state$method, state$method_args, state$sim, state$minimum_cases,
        input$measure, input$alternative, input$iter, input$cores, state$seed,
        progress_file()
      )

    })

    # Watch for reuse button
    observeEvent(input$run_again, {

      # Require previous result
      req(state$perm_result, input$measure)

      # Create progress file
      progress_file(new_progress_file())

      # Start task
      reuse_task$invoke(state$perm_result, input$measure, input$alternative, input$cores, progress_file())

    })

    # Watch for finished task
    observe({

      # Check for success
      if(task$status() == "success"){

        # Store result
        state$perm_result <- task$result()

      }

    })

    # Watch for finished reuse task
    observe({

      # Check for success
      if(reuse_task$status() == "success"){

        # Store result
        state$perm_result <- reuse_task$result()

      }

    })

    # Watch for plot button
    observeEvent(input$run_plot, {

      # Require result
      req(state$perm_result)

      # Render plot
      output$perm_plot <- renderPlot({

        # Require result and names (cleared on reset)
        req(state$perm_result, sample_names())

        # Get result
        result <- state$perm_result

        # Name samples
        names(result$result)[1:2] <- sample_names()

        # Name first sample in alternative
        result$result$alternative <- gsub("sample_1", sample_names()[1], result$result$alternative, fixed = TRUE)

        # Name second sample in alternative
        result$result$alternative <- gsub("sample_2", sample_names()[2], result$result$alternative, fixed = TRUE)

        # Plot permutation
        plot(result)

      })

    })

    # Render status
    output$status <- renderUI({

      # Check for running task
      if(task$status() == "running"){

        # Show progress of task
        return(progress_status_ui(task, progress))

      }

      # Check for running reuse task
      if(reuse_task$status() == "running"){

        # Show progress of reuse task
        return(progress_status_ui(reuse_task, progress))

      }

      # Show status of both tasks
      tagList(progress_status_ui(task, progress), progress_status_ui(reuse_task, progress))

    })

    # Render results
    output$result <- renderUI({

      # Require result and names
      req(state$perm_result, sample_names())

      # Get result
      result <- state$perm_result

      # Name samples
      names(result$result)[1:2] <- sample_names()

      # Name first sample in alternative
      result$result$alternative <- gsub("sample_1", sample_names()[1], result$result$alternative, fixed = TRUE)

      # Name second sample in alternative
      result$result$alternative <- gsub("sample_2", sample_names()[2], result$result$alternative, fixed = TRUE)

      # Show test as it prints in the console
      console_display_ui(semnet_internal("permutation_display")(result))

    })

  })

}
