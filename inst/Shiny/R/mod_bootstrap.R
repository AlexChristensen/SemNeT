# Bootstrap Analyses tab: calls bootstrap_SemNeT() (once per requested node
# proportion, or once for a case bootstrap), then bootstrap_test_SemNeT()
# and plot.bootstrap_SemNeT() directly on the result(s). `keep_networks` is
# never set to TRUE here -- this tab never needs the raw bootstrapped
# networks themselves, only their measures (see SHINY_PLAN.md's "Memory &
# speed measures").

mod_bootstrap_ui <- function(id)
{

  ns <- NS(id)

  tagList(

    sidebarPanel(

      selectInput(ns("type"), "Type of Bootstrap", choices = c("Case" = "case", "Node" = "node")),

      uiOutput(ns("node_options")),

      selectInput(ns("test"), "Statistical Test", choices = c("t-test", "ANOVA", "ANCOVA")),

      numericInput(ns("iter"), "Number of Iterations", value = 1000, min = 1, max = 20000, step = 100),
      numericInput(ns("cores"), "Cores", value = 1, min = 1, max = max_app_cores()),

      actionButton(ns("run_boot"), label = "Perform Bootstrap Analyses", class = "btn-primary"),
      actionButton(ns("run_plot"), label = "Generate Bootstrap Plots")

    ),

    mainPanel(
      uiOutput(ns("status")),
      tableOutput(ns("test_table")),
      uiOutput(ns("plot_selector")),
      plotOutput(ns("boot_plot"))
    ),

    tags$footer(
      HTML(paste0(
        "<b>Please cite:</b><br>",
        "Kenett, Y. N., Anaki, D., & Faust, M. (2014). Investigating the structure of semantic networks in low ",
        "and high creative persons. <em>Frontiers in Human Neuroscience</em>, <em>8</em>, 407."
      )),
      style = "margin: 20px;"
    )

  )

}

mod_bootstrap_server <- function(id, state)
{

  moduleServer(id, function(input, output, session){

    output$node_options <- renderUI({

      ns <- session$ns

      if(input$type == "node"){
        checkboxGroupInput(
          ns("percent"), "Proportion of Nodes Remaining",
          choiceNames = sprintf("%1.2f", seq(.5, .9, .1)),
          choiceValues = seq(.5, .9, .1), inline = TRUE, selected = c(.5, .7, .9)
        )
      }

    })

    progress_file <- reactiveVal(NULL)
    progress <- progress_poller(session, progress_file)

    task <- ExtendedTask$new(function(groups_data, type, method, method_args, sim, minimum_cases, percents, iter, cores, progress_file){
      promises::future_promise({

        # See mod_shared.R's `progress_poller()` for why this (not `progressr`)
        # is what actually gets live progress out of a background worker
        options(SemNeT.progress_file = progress_file)

        results <- lapply(percents, function(prop){
          args <- c(
            groups_data,
            list(
              method = method, method_args = method_args, type = type,
              sim = sim, minimum_cases = minimum_cases, iter = iter, cores = cores
            )
          )
          if(type == "node"){
            args$prop <- prop
          }
          do.call(bootstrap_SemNeT, args)
        })

        names(results) <- if(type == "node") sprintf("%1.2f", percents) else "Case"

        results

      }, seed = TRUE)
    })

    observeEvent(input$run_boot, {

      req(state$groups_data, state$method)

      percents <- if(input$type == "node"){
        req(input$percent)
        as.numeric(input$percent)
      }else{
        1
      }

      warn_if_slow(input$iter * length(percents))

      progress_file(new_progress_file())

      task$invoke(
        state$groups_data, input$type, state$method, state$method_args,
        state$sim, state$minimum_cases, percents, input$iter, input$cores,
        progress_file()
      )

    })

    observe({
      if(task$status() == "success"){
        state$boot_result <- task$result()
        state$boot_test <- NULL
      }
    })

    output$status <- renderUI(progress_status_ui(task, progress))

    observeEvent(list(state$boot_result, input$test), {

      req(state$boot_result)

      state$boot_test <- tryCatch(
        do.call(bootstrap_test_SemNeT, c(state$boot_result, list(test = input$test))),
        error = function(e){
          shinyalert::shinyalert(title = "Statistical test failed", text = conditionMessage(e), type = "error")
          NULL
        }
      )

    })

    output$test_table <- renderTable({

      req(state$boot_test)

      do.call(rbind, lapply(names(state$boot_test$results), function(condition){
        do.call(rbind, lapply(names(state$boot_test$results[[condition]]), function(measure){
          entry <- state$boot_test$results[[condition]][[measure]]
          cbind(Condition = condition, Measure = measure, entry$table)
        }))
      }))

    })

    output$plot_selector <- renderUI({
      req(state$boot_result)
      if(length(state$boot_result) > 1){
        ns <- session$ns
        selectInput(ns("plot_condition"), "Proportion to Plot", choices = names(state$boot_result))
      }
    })

    observeEvent(input$run_plot, {

      req(state$boot_result)
      condition <- if(!is.null(input$plot_condition)) input$plot_condition else names(state$boot_result)[1]

      output$boot_plot <- renderPlot({
        # The generic dispatches correctly via the registered S3 method even
        # though `plot.bootstrap_SemNeT()` itself isn't exported -- calling
        # it directly by name (as this used to) only worked under
        # `devtools::load_all()`, which attaches internals too; it fails
        # under a real `library(SemNeT)` install, exactly how the Shiny app
        # actually launches
        plot(state$boot_result[[condition]])
      })

    })

  })

}
