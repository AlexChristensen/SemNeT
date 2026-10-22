# Spreading activation module ----
# Updated 22.10.2026

# Spreading activation interface ----
# Updated 13.10.2026
mod_spreading_activation_ui <- function(id)
{

  # Set namespace
  ns <- NS(id)

  # Return interface
  tagList(

    # Add sidebar
    sidebarPanel(

      # Add network selector
      uiOutput(ns("network_selector")),

      # Add retention
      numericInput(ns("retention"), "Retention (proportion of activation kept)", value = .5, min = 0, max = 1, step = .05),

      # Add time steps
      numericInput(ns("time"), "Number of Time Steps", value = 10, min = 1, max = 200, step = 1),

      # Add decay
      numericInput(ns("decay"), "Decay (activation lost per time step)", value = 0, min = 0, max = 1, step = .05),

      # Add suppression
      numericInput(ns("suppress"), "Suppress (activation below this is zeroed)", value = 0, min = 0, max = 1, step = .001),

      # Add activation button
      actionButton(ns("set_activation"), label = "Set Starting Activations"),

      # Add activation modal
      shinyBS::bsModal(
        ns("activation_modal"), title = "Starting Node Activations", trigger = ns("set_activation"), size = "large",
        uiOutput(ns("activation_matrix"))
      ),

      # Add run button
      actionButton(ns("run"), label = "Run Spreading Activation", class = "btn-primary"),

      # Add reset button
      actionButton(ns("reset"), label = "Reset")

    ),

    # Add main panel
    mainPanel(

      # Add status
      uiOutput(ns("status")),

      # Add time slider
      uiOutput(ns("time_slider")),

      # Add plot
      plotOutput(ns("frame")),

      # Add activation table (the time step's rows of the console output)
      tableOutput(ns("frame_table"))

    ),

    # Add citation footer
    tags$footer(
      HTML(paste0(
        "<b>Please cite:</b><br>",
        "Siew, C. S. Q. (2019). spreadr: An R package to simulate spreading activation in a network. ",
        "<em>Behavior Research Methods</em>, <em>51</em>, 910–929."
      )),
      style = "margin: 20px;"
    )

  )

}

# Spreading activation server ----
# Updated 22.10.2026
mod_spreading_activation_server <- function(id, state)
{

  # Start module
  moduleServer(id, function(input, output, session){

    # Render network selector
    output$network_selector <- renderUI({

      # Require networks
      req(state$networks)

      # Get namespace
      ns <- session$ns

      # Show selector
      selectInput(ns("network"), "Select a Network", choices = names(state$networks))

    })

    # Render activation matrix
    output$activation_matrix <- renderUI({

      # Require networks and selection
      req(state$networks, input$network)

      # Get namespace
      ns <- session$ns

      # Get nodes
      nodes <- colnames(state$networks[[input$network]])

      # Show matrix input
      shinyMatrix::matrixInput(
        ns("node_activation"),
        cols = list(names = TRUE, editableNames = FALSE),
        value = matrix(cbind(nodes, 0), ncol = 2, dimnames = list(NULL, c("Node", "Activation")))
      )

    })

    # Set up background task
    task <- ExtendedTask$new(function(network, start_run, retention, time, decay, suppress){

      # Run in background
      promises::future_promise({

        # Simulate spreading activation
        spreadr::spreadr(
          network = abs(network), start_run = start_run,
          retention = retention, time = time, decay = decay, suppress = suppress
        )

      }, seed = TRUE)

    })

    # Watch for run button
    observeEvent(input$run, {

      # Require networks, selection, and activations
      req(state$networks, input$network, input$node_activation)

      # Get activations
      activation <- as.data.frame(input$node_activation, stringsAsFactors = FALSE)

      # Name columns
      colnames(activation) <- c("node", "activation")

      # Convert activations to numbers
      activation$activation <- suppressWarnings(as.numeric(activation$activation))

      # Keep active nodes
      activation <- activation[!is.na(activation$activation) & activation$activation != 0, ]

      # Check for no active nodes
      if(nrow(activation) == 0){

        # Send warning alert
        shinyalert::shinyalert(title = "Set at least one node's starting activation above 0", type = "warning")

        # Return nothing
        return(invisible(NULL))

      }

      # Start task
      task$invoke(state$networks[[input$network]], activation, input$retention, input$time, input$decay, input$suppress)

    })

    # Watch for finished task
    observe({

      # Check for success
      if(task$status() == "success"){

        # Store result
        state$spreading_result <- list(network = state$networks[[input$network]], activation = task$result())

      }

    })

    # Watch for reset button
    observeEvent(input$reset, {

      # Clear result
      state$spreading_result <- NULL

    })

    # Render status
    output$status <- renderUI(task_status_ui(task))

    # Render time slider
    output$time_slider <- renderUI({

      # Require result
      req(state$spreading_result)

      # Get namespace
      ns <- session$ns

      # Show slider
      sliderInput(ns("time_step"), "Time Step", min = 1, max = max(state$spreading_result$activation$time), value = 1, step = 1, animate = TRUE)

    })

    # Render plot
    output$frame <- renderPlot({

      # Require result and time step
      req(state$spreading_result, input$time_step)

      # Plot frame
      plot_spreading_frame(state$spreading_result$network, state$spreading_result$activation, input$time_step)

    })

    # Render activation table
    output$frame_table <- renderTable({

      # Require result and time step
      req(state$spreading_result, input$time_step)

      # Get activation of time step
      frame <- state$spreading_result$activation
      frame <- frame[frame$time == input$time_step, c("node", "activation", "time")]

      # Order nodes by activation
      frame <- frame[order(-frame$activation), ]

      # Name columns
      names(frame) <- c("Node", "Activation", "Time Step")

      # Return activation
      frame

    }, rownames = FALSE, digits = 4, caption = paste(
      "Activation of each node at the selected time step (the rows of the spreadr output for that time step)."
    ), caption.placement = "top")

  })

}
