# Bootstrap module ----
# Updated 13.10.2026

# Bootstrap interface ----
# Updated 13.10.2026
mod_bootstrap_ui <- function(id)
{

  # Set namespace
  ns <- NS(id)

  # Return interface
  tagList(

    # Add sidebar
    sidebarPanel(

      # Add type selection
      selectInput(ns("type"), "Type of Bootstrap", choices = c("Case" = "case", "Node" = "node")),

      # Add node options
      uiOutput(ns("node_options")),

      # Add confidence level
      numericInput(ns("ci_level"), "Confidence Level", value = .95, min = .5, max = .999, step = .01),

      # Add interval type
      selectInput(ns("ci_type"), "Interval Type", choices = c("Normal" = "normal", "Bias-corrected percentile" = "bc", "Percentile" = "percentile")),

      # Add adjustment
      selectInput(ns("adjust"), "Multiplicity Adjustment", choices = c("None" = "none", "Bonferroni" = "bonferroni")),

      # Add iterations
      numericInput(ns("iter"), "Number of Iterations", value = 1000, min = 1, max = 20000, step = 100),

      # Add cores
      numericInput(ns("cores"), "Cores", value = 1, min = 1, max = max_app_cores()),

      # Add run button
      actionButton(ns("run_boot"), label = "Perform Bootstrap Analyses", class = "btn-primary"),

      # Add plot button
      actionButton(ns("run_plot"), label = "Generate Bootstrap Plots")

    ),

    # Add main panel
    mainPanel(

      # Add status
      uiOutput(ns("status")),

      # Add notes
      uiOutput(ns("test_notes")),

      # Add test table
      tableOutput(ns("test_table")),

      # Add plot selector
      uiOutput(ns("plot_selector")),

      # Add plot
      plotOutput(ns("boot_plot"))

    ),

    # Add citation footer
    tags$footer(
      HTML(paste0(
        "<b>Please cite:</b><br>",
        "Kenett, Y. N., Anaki, D., & Faust, M. (2014). Investigating the structure of semantic networks in low ",
        "and high creative persons. <em>Frontiers in Human Neuroscience</em>, <em>8</em>, 407.<br>",
        "Tu, C.-Y., Chodorow, M., Brooks, P. J., Ezrina, E., & Valian, V. (under review). Inflated statistics lead to ",
        "incorrect inference: How (not) to make cognitive network group comparisons."
      )),
      style = "margin: 20px;"
    )

  )

}

# Bootstrap server ----
# Updated 13.10.2026
mod_bootstrap_server <- function(id, state)
{

  # Start module
  moduleServer(id, function(input, output, session){

    # Render node options
    output$node_options <- renderUI({

      # Get namespace
      ns <- session$ns

      # Check for node type
      if(input$type == "node"){

        # Show proportions of nodes
        checkboxGroupInput(
          ns("percent"), "Proportion of Nodes Remaining",
          choiceNames = sprintf("%1.2f", seq(.5, .9, .1)),
          choiceValues = seq(.5, .9, .1), inline = TRUE, selected = c(.5, .7, .9)
        )

      }

    })

    # Set up progress file
    progress_file <- reactiveVal(NULL)

    # Poll progress
    progress <- progress_poller(session, progress_file)

    # Set up background task
    task <- ExtendedTask$new(function(groups_data, type, method, method_args, sim, minimum_cases, percents, iter, cores, progress_file){

      # Run in background
      promises::future_promise({

        # Set progress file
        options(SemNeT.progress_file = progress_file)

        # Loop over proportions
        results <- lapply(percents, function(prop){

          # Set up arguments
          args <- c(
            groups_data,
            list(
              method = method, method_args = method_args, type = type,
              sim = sim, minimum_cases = minimum_cases, iter = iter, cores = cores
            )
          )

          # Check for node type
          if(type == "node"){

            # Add proportion
            args$prop <- prop

          }

          # Perform bootstrap
          do.call(bootstrap_SemNeT, args)

        })

        # Name results
        names(results) <- if(type == "node") sprintf("%1.2f", percents) else "Case"

        # Return results
        results

      }, seed = TRUE)

    })

    # Watch for run button
    observeEvent(input$run_boot, {

      # Require groups and method
      req(state$groups_data, state$method)

      # Get proportions
      percents <- if(input$type == "node"){

        # Require proportions
        req(input$percent)

        # Use selected proportions
        as.numeric(input$percent)

      }else{

        # Use full proportion
        1

      }

      # Warn about many iterations
      warn_if_slow(input$iter * length(percents))

      # Create progress file
      progress_file(new_progress_file())

      # Start task
      task$invoke(
        state$groups_data, input$type, state$method, state$method_args,
        state$sim, state$minimum_cases, percents, input$iter, input$cores,
        progress_file()
      )

    })

    # Watch for finished task
    observe({

      # Check for success
      if(task$status() == "success"){

        # Store result
        state$boot_result <- task$result()

        # Clear tests
        state$boot_test <- NULL

      }

    })

    # Render status
    output$status <- renderUI(progress_status_ui(task, progress))

    # Set up notes of test
    test_notes <- reactiveVal(character(0))

    # Watch for result and test options
    observeEvent(list(state$boot_result, input$ci_level, input$adjust, input$ci_type), {

      # Require result and level
      req(state$boot_result, input$ci_level)

      # Initialize notes
      notes <- character(0)

      # Perform tests
      state$boot_test <- withCallingHandlers(
        tryCatch(
          do.call(
            bootstrap_test_SemNeT,
            c(
              state$boot_result,
              list(
                ci_level = input$ci_level, adjust = input$adjust, ci_type = input$ci_type
              )
            )
          ),
          error = function(e){

            # Send error alert
            shinyalert::shinyalert(title = "Statistical test failed", text = conditionMessage(e), type = "error")

            # Return nothing
            NULL

          }
        ),
        warning = function(w){

          # Store warning
          notes <<- c(notes, conditionMessage(w))

          # Muffle warning
          invokeRestart("muffleWarning")

        }
      )

      # Store notes
      test_notes(notes)

    })

    # Render notes
    output$test_notes <- renderUI({

      # Require test
      req(state$boot_test)

      # Check for node type
      is_node <- names(state$boot_result)[1] != "Case"

      # Show notes
      tagList(
        p(
          em(
            if(is_node){

              # Show note on node subsampling
              paste(
                "Node (vocabulary) subsampling: these summaries describe how robust each group difference is to",
                "which words are included, at a reduced network size. They are not confidence intervals and do not",
                "support inference about groups of participants -- use a Case bootstrap for that."
              )

            }else{

              # Show note on case resampling
              paste(
                "Bootstrap intervals of within-iteration group differences. A difference is reliable when",
                "its interval excludes 0; a wide interval that includes 0 means the data are inconclusive, not that",
                "the groups are equivalent.",
                if(identical(input$ci_type, "normal")){

                  # Show note on Wald test
                  paste(
                    "With the Normal interval, z, p, and the minimal detectable effect come from a Wald test of the observed",
                    "difference using the bootstrap standard error (assumes a roughly normal sampling distribution;",
                    "not adjusted across measures)."
                  )

                }else{

                  # Show note on percentile intervals
                  "Percentile intervals have no p-value."

                },
                "For a formal hypothesis test, use the Permutation Analyses tab."
              )

            }
          )
        ),
        lapply(test_notes(), function(note) div(class = "alert alert-warning", note)),
        div(
          class = "alert alert-warning",
          "The Bootstrap and Permutation Analyses should always be run together. Run the Permutation Analyses as well and report both."
        )
      )

    })

    # Render test table
    output$test_table <- renderTable({

      # Require test
      req(state$boot_test)

      # Combine tables of conditions
      do.call(rbind, lapply(names(state$boot_test$results), function(condition){

        # Combine tables of measures
        do.call(rbind, lapply(names(state$boot_test$results[[condition]]), function(measure){

          # Get entry
          entry <- state$boot_test$results[[condition]][[measure]]

          # Add condition and measure
          cbind(Condition = condition, Measure = measure, entry$table)

        }))

      }))

    }, digits = 4)

    # Render plot selector
    output$plot_selector <- renderUI({

      # Require result
      req(state$boot_result)

      # Check for multiple conditions
      if(length(state$boot_result) > 1){

        # Get namespace
        ns <- session$ns

        # Show selector
        selectInput(ns("plot_condition"), "Proportion to Plot", choices = names(state$boot_result))

      }

    })

    # Watch for plot button
    observeEvent(input$run_plot, {

      # Require result
      req(state$boot_result)

      # Get condition to plot
      condition <- if(!is.null(input$plot_condition)) input$plot_condition else names(state$boot_result)[1]

      # Render plot
      output$boot_plot <- renderPlot({

        # Plot bootstrap
        plot(state$boot_result[[condition]])

      })

    })

  })

}
