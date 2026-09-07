# Permutation Analyses tab (new -- the original had this entirely disabled,
# since permSemNeT() had real bugs and wasn't exported; permutation_SemNeT()
# is solid now). Calls permutation_SemNeT() directly. `keep_networks = TRUE`
# is used here (unlike the Bootstrap tab) specifically so "Test Another
# Measure" can reuse the same permuted networks via `previous =`.

mod_permutation_ui <- function(id)
{

  ns <- NS(id)

  tagList(

    sidebarPanel(

      uiOutput(ns("group_selectors")),

      selectInput(ns("measure"), "Network Measure", choices = c("ASPL" = "aspl", "CC" = "cc", "Q" = "q")),
      selectInput(
        ns("alternative"), "Alternative Hypothesis",
        choices = c("Two-tailed" = "two_tailed", "Less" = "less", "Greater" = "greater")
      ),

      numericInput(ns("iter"), "Number of Iterations", value = 1000, min = 1, max = 20000, step = 100),
      numericInput(ns("cores"), "Cores", value = 1, min = 1, max = max_app_cores()),

      actionButton(ns("run"), label = "Perform Permutation Analysis", class = "btn-primary"),
      actionButton(ns("run_again"), label = "Test Another Measure (reuses permuted networks)")

    ),

    mainPanel(
      uiOutput(ns("status")),
      tableOutput(ns("result"))
    )

  )

}

mod_permutation_server <- function(id, state)
{

  moduleServer(id, function(input, output, session){

    output$group_selectors <- renderUI({
      req(state$groups_data)
      ns <- session$ns
      names <- names(state$groups_data)
      tagList(
        selectInput(ns("sample_1"), "Sample 1", choices = names, selected = names[1]),
        selectInput(ns("sample_2"), "Sample 2", choices = names, selected = names[min(2, length(names))])
      )
    })

    task <- ExtendedTask$new(function(sample_1, sample_2, method, method_args, sim, minimum_cases, measure, alternative, iter, cores){
      promises::future_promise({
        permutation_SemNeT(
          sample_1 = sample_1, sample_2 = sample_2, method = method, method_args = method_args,
          sim = sim, minimum_cases = minimum_cases, measure = measure, alternative = alternative,
          iter = iter, cores = cores, keep_networks = TRUE
        )
      }, seed = TRUE)
    })

    reuse_task <- ExtendedTask$new(function(previous, measure, alternative, cores){
      promises::future_promise({
        permutation_SemNeT(previous = previous, measure = measure, alternative = alternative, cores = cores)
      }, seed = TRUE)
    })

    # `permutation_SemNeT()` labels its result columns from `substitute()` on
    # its own call-site argument names -- inside the ExtendedTask's closure
    # those are always literally `sample_1`/`sample_2`, not the user's chosen
    # group names, so the real names are tracked here and used to relabel the
    # displayed table instead
    sample_names <- reactiveVal(NULL)

    observeEvent(input$run, {

      req(state$groups_data, state$method, input$sample_1, input$sample_2)

      if(input$sample_1 == input$sample_2){
        shinyalert::shinyalert(title = "Pick two different samples", type = "warning")
        return(invisible(NULL))
      }

      warn_if_slow(input$iter)
      sample_names(c(input$sample_1, input$sample_2))

      task$invoke(
        state$groups_data[[input$sample_1]], state$groups_data[[input$sample_2]],
        state$method, state$method_args, state$sim, state$minimum_cases,
        input$measure, input$alternative, input$iter, input$cores
      )

    })

    observeEvent(input$run_again, {
      req(state$perm_result)
      reuse_task$invoke(state$perm_result, input$measure, input$alternative, input$cores)
    })

    observe({
      if(task$status() == "success"){
        state$perm_result <- task$result()
      }
    })

    observe({
      if(reuse_task$status() == "success"){
        state$perm_result <- reuse_task$result()
      }
    })

    output$status <- renderUI({
      if(task$status() == "running" || reuse_task$status() == "running"){
        return(tags$p(tags$em("Running... this tab will update when it's done.")))
      }
      tagList(task_status_ui(task), task_status_ui(reuse_task))
    })

    output$result <- renderTable({

      req(state$perm_result, sample_names())

      result <- state$perm_result$result
      names(result)[1:2] <- sample_names()
      result$alternative <- gsub("sample_1", sample_names()[1], result$alternative, fixed = TRUE)
      result$alternative <- gsub("sample_2", sample_names()[2], result$alternative, fixed = TRUE)

      result

    }, rownames = TRUE)

  })

}
