# Random Network Analyses tab: calls random_network_test() directly on the
# networks estimated in the Network Estimation tab. Runs via ExtendedTask so
# a long run doesn't block the session's UI.

mod_random_network_ui <- function(id)
{

  ns <- NS(id)

  tagList(

    sidebarPanel(

      numericInput(ns("iter"), "Number of Iterations", value = 1000, min = 1, max = 20000, step = 100),
      numericInput(ns("cores"), "Cores", value = 1, min = 1, max = max_app_cores()),

      actionButton(ns("run"), label = "Perform Random Network Analyses", class = "btn-primary")

    ),

    mainPanel(
      uiOutput(ns("status")),
      tableOutput(ns("result"))
    ),

    tags$footer(
      HTML(paste0(
        "<b>Please cite:</b><br>",
        "Viger, F., & Latapy, M. (2016). Efficient and simple generation of random simple connected graphs ",
        "with prescribed degree sequence. <em>Journal of Complex Networks</em>, <em>4</em>, 15–37."
      )),
      style = "margin: 20px;"
    )

  )

}

mod_random_network_server <- function(id, state)
{

  moduleServer(id, function(input, output, session){

    task <- ExtendedTask$new(function(networks, iter, cores){
      promises::future_promise({
        do.call(random_network_test, c(networks, list(iter = iter, cores = cores)))
      }, seed = TRUE)
    })

    observeEvent(input$run, {
      req(state$networks)
      warn_if_slow(input$iter)
      task$invoke(state$networks, input$iter, input$cores)
    })

    observe({
      if(task$status() == "success"){
        state$random_test <- task$result()
      }
    })

    output$status <- renderUI(task_status_ui(task))

    output$result <- renderTable({
      req(state$random_test)
      do.call(rbind, lapply(names(state$random_test), function(name){
        cbind(Group = name, Measure = row.names(state$random_test[[name]]), state$random_test[[name]])
      }))
    })

  })

}
