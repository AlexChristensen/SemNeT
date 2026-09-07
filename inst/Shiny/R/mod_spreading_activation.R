# Spreading Activation tab: calls spreadr::spreadr() directly on one of the
# estimated networks. No SemNeT algorithm is involved here at all (spreadr is
# an independent package), so there's no package-function duplication to
# eliminate -- only the plotting glue in `plot_spreading_frame()`
# (mod_shared.R) is app-specific.

mod_spreading_activation_ui <- function(id)
{

  ns <- NS(id)

  tagList(

    sidebarPanel(

      uiOutput(ns("network_selector")),

      numericInput(ns("retention"), "Retention (proportion of activation kept)", value = .5, min = 0, max = 1, step = .05),
      numericInput(ns("time"), "Number of Time Steps", value = 10, min = 1, max = 200, step = 1),
      numericInput(ns("decay"), "Decay (activation lost per time step)", value = 0, min = 0, max = 1, step = .05),
      numericInput(ns("suppress"), "Suppress (activation below this is zeroed)", value = 0, min = 0, max = 1, step = .001),

      actionButton(ns("set_activation"), label = "Set Starting Activations"),

      shinyBS::bsModal(
        ns("activation_modal"), title = "Starting Node Activations", trigger = ns("set_activation"), size = "large",
        uiOutput(ns("activation_matrix"))
      ),

      actionButton(ns("run"), label = "Run Spreading Activation", class = "btn-primary"),
      actionButton(ns("reset"), label = "Reset")

    ),

    mainPanel(
      uiOutput(ns("status")),
      uiOutput(ns("time_slider")),
      plotOutput(ns("frame"))
    ),

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

mod_spreading_activation_server <- function(id, state)
{

  moduleServer(id, function(input, output, session){

    output$network_selector <- renderUI({
      req(state$networks)
      ns <- session$ns
      selectInput(ns("network"), "Select a Network", choices = names(state$networks))
    })

    output$activation_matrix <- renderUI({

      req(state$networks, input$network)
      ns <- session$ns
      nodes <- colnames(state$networks[[input$network]])

      shinyMatrix::matrixInput(
        ns("node_activation"),
        cols = list(names = TRUE, editableNames = FALSE),
        value = matrix(cbind(nodes, 0), ncol = 2, dimnames = list(NULL, c("Node", "Activation")))
      )

    })

    task <- ExtendedTask$new(function(network, start_run, retention, time, decay, suppress){
      promises::future_promise({
        # `spreadr::spreadr()` requires a non-negative adjacency matrix (it
        # spreads a proportion of each node's activation along its edges,
        # which is undefined for a negative weight) -- a TMFG network
        # estimated from correlations can have negative edges, so its
        # magnitude (not sign) is what determines how strongly two nodes
        # are connected for spreading purposes
        spreadr::spreadr(
          network = abs(network), start_run = start_run,
          retention = retention, time = time, decay = decay, suppress = suppress
        )
      }, seed = TRUE)
    })

    observeEvent(input$run, {

      req(state$networks, input$network, input$node_activation)

      activation <- as.data.frame(input$node_activation, stringsAsFactors = FALSE)
      colnames(activation) <- c("node", "activation")
      activation$activation <- suppressWarnings(as.numeric(activation$activation))
      activation <- activation[!is.na(activation$activation) & activation$activation != 0, ]

      if(nrow(activation) == 0){
        shinyalert::shinyalert(title = "Set at least one node's starting activation above 0", type = "warning")
        return(invisible(NULL))
      }

      task$invoke(state$networks[[input$network]], activation, input$retention, input$time, input$decay, input$suppress)

    })

    observe({
      if(task$status() == "success"){
        state$spreading_result <- list(network = state$networks[[input$network]], activation = task$result())
      }
    })

    observeEvent(input$reset, {
      state$spreading_result <- NULL
    })

    output$status <- renderUI(task_status_ui(task))

    output$time_slider <- renderUI({
      req(state$spreading_result)
      ns <- session$ns
      sliderInput(ns("time_step"), "Time Step", min = 1, max = max(state$spreading_result$activation$time), value = 1, step = 1, animate = TRUE)
    })

    output$frame <- renderPlot({
      req(state$spreading_result, input$time_step)
      plot_spreading_frame(state$spreading_result$network, state$spreading_result$activation, input$time_step)
    })

  })

}
