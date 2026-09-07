# Export tab: replaces the original's "assign into your live R session"
# save pattern (which only worked for a single local user) with real
# downloads -- required for a hosted, multi-user deployment where there's no
# shared R session to assign into, and it's what lets a session's `state`
# be safely dropped rather than accumulating indefinitely.

mod_export_ui <- function(id)
{

  ns <- NS(id)

  tagList(

    sidebarPanel(

      tags$p("Download whatever this session has computed so far."),

      downloadButton(ns("download_all"), "All Results (.rds)"),
      br(), br(),
      downloadButton(ns("download_networks"), "Estimated Networks (.rds)"),
      br(), br(),
      downloadButton(ns("download_measures"), "Network Measures (.csv)"),
      br(), br(),
      downloadButton(ns("download_boot_test"), "Bootstrap Test Table (.csv)"),
      br(), br(),
      downloadButton(ns("download_random_walk"), "Random Walk Table (.csv)"),

      br(), br(),
      actionButton(ns("reset"), label = "Clear This Session's Results", class = "btn-danger")

    ),

    mainPanel(
      tags$p("Available in this session:"),
      tableOutput(ns("summary"))
    )

  )

}

mod_export_server <- function(id, state)
{

  moduleServer(id, function(input, output, session){

    output$summary <- renderTable({

      fields <- c(
        "data", "networks", "random_test", "boot_result", "boot_test",
        "perm_result", "random_walk", "spreading_result"
      )

      data.frame(
        Result = fields,
        Available = vapply(fields, function(field) !is.null(state[[field]]), logical(1))
      )

    })

    output$download_all <- downloadHandler(
      filename = function() "semnet_results.rds",
      content = function(file){
        saveRDS(reactiveValuesToList(state), file)
      }
    )

    output$download_networks <- downloadHandler(
      filename = function() "semnet_networks.rds",
      content = function(file){
        req(state$networks)
        saveRDS(state$networks, file)
      }
    )

    output$download_measures <- downloadHandler(
      filename = function() "semnet_measures.csv",
      content = function(file){
        req(state$measures)
        utils::write.csv(state$measures, file)
      }
    )

    output$download_boot_test <- downloadHandler(
      filename = function() "semnet_bootstrap_test.csv",
      content = function(file){
        req(state$boot_test)
        table <- do.call(rbind, lapply(names(state$boot_test$results), function(condition){
          do.call(rbind, lapply(names(state$boot_test$results[[condition]]), function(measure){
            entry <- state$boot_test$results[[condition]][[measure]]
            cbind(Condition = condition, Measure = measure, entry$table)
          }))
        }))
        utils::write.csv(table, file, row.names = FALSE)
      }
    )

    output$download_random_walk <- downloadHandler(
      filename = function() "semnet_random_walk.csv",
      content = function(file){
        req(state$random_walk)
        utils::write.csv(state$random_walk$short, file, row.names = FALSE)
      }
    )

    observeEvent(input$reset, {

      shinyalert::shinyalert(
        title = "Clear this session's results?",
        text = "This can't be undone. Your uploaded data stays, but every analysis result is cleared.",
        type = "warning", showCancelButton = TRUE, confirmButtonText = "Clear",
        callbackR = function(confirmed){
          if(isTRUE(confirmed)){
            for(field in c("networks", "measures", "random_test", "boot_result", "boot_test", "perm_result", "random_walk", "spreading_result")){
              state[[field]] <- NULL
            }
          }
        }
      )

    })

  })

}
