# Random Walk Analyses tab: calls random_walk() directly on two of the
# networks estimated in the Network Estimation tab.

mod_random_walk_ui <- function(id)
{

  ns <- NS(id)

  tagList(

    sidebarPanel(

      uiOutput(ns("network_selectors")),

      numericInput(ns("steps"), "Starting Number of Steps", value = 10, min = 1, step = 1),
      numericInput(ns("reps"), "Number of Repetitions (each increases steps by 10)", value = 20, min = 1, max = 50, step = 5),
      numericInput(ns("iter"), "Number of Iterations", value = 10000, min = 1, max = 50000, step = 1000),
      numericInput(ns("cores"), "Cores", value = 1, min = 1, max = max_app_cores()),

      actionButton(ns("run"), label = "Perform Random Walk Analyses", class = "btn-primary")

    ),

    mainPanel(
      uiOutput(ns("status")),
      tableOutput(ns("result"))
    ),

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

mod_random_walk_server <- function(id, state)
{

  moduleServer(id, function(input, output, session){

    output$network_selectors <- renderUI({
      req(state$networks)
      ns <- session$ns
      names <- names(state$networks)
      tagList(
        selectInput(ns("network_a"), "Select Network A", choices = names, selected = names[1]),
        selectInput(ns("network_b"), "Select Network B", choices = names, selected = names[min(2, length(names))])
      )
    })

    task <- ExtendedTask$new(function(network_a, network_b, name_a, name_b, reps, steps, iter, cores){
      promises::future_promise({
        random_walk(network_a, network_b, name_a = name_a, name_b = name_b, reps = reps, steps = steps, iter = iter, cores = cores)
      }, seed = TRUE)
    })

    observeEvent(input$run, {

      req(state$networks, input$network_a, input$network_b)

      if(input$network_a == input$network_b){
        shinyalert::shinyalert(title = "Pick two different networks", type = "warning")
        return(invisible(NULL))
      }

      warn_if_slow(input$iter * input$reps)

      task$invoke(
        state$networks[[input$network_a]], state$networks[[input$network_b]],
        input$network_a, input$network_b, input$reps, input$steps, input$iter, input$cores
      )

    })

    observe({
      if(task$status() == "success"){
        state$random_walk <- task$result()
      }
    })

    output$status <- renderUI(task_status_ui(task))

    output$result <- renderTable({
      req(state$random_walk)
      state$random_walk$short
    }, rownames = FALSE, caption = "Random Walk Results", caption.placement = "top")

  })

}
