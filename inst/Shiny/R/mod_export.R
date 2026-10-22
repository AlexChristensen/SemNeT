# Export module ----
# Updated 22.10.2026

# Export interface ----
# Updated 22.10.2026
mod_export_ui <- function(id)
{

  # Set namespace
  ns <- NS(id)

  # Return interface
  tagList(

    # Add sidebar
    sidebarPanel(

      # Add note
      tags$p("Download whatever this session has computed so far."),

      # Add download buttons, shown once results exist
      uiOutput(ns("downloads"))

    ),

    # Add main panel
    mainPanel(

      # Add note
      tags$p("Available in this session:"),

      # Add summary table
      tableOutput(ns("summary"))

    )

  )

}

# Export server ----
# Updated 22.10.2026
mod_export_server <- function(id, state)
{

  # Start module
  moduleServer(id, function(input, output, session){

    # Render summary
    output$summary <- renderTable({

      # Set fields of results
      fields <- c(
        "data", "networks", "random_test", "boot_result", "boot_test",
        "perm_result", "random_walk", "spreading_result"
      )

      # Return availability of fields
      data.frame(
        Result = fields,
        Available = vapply(fields, function(field) !is.null(state[[field]]), logical(1))
      )

    })

    # Render download buttons for completed results
    output$downloads <- renderUI({

      # Get namespace
      ns <- session$ns

      # Set buttons for each result
      buttons <- list(
        download_networks = list(field = "networks", label = "Estimated Networks (.rds)"),
        download_measures = list(field = "measures", label = "Network Measures (.csv)"),
        download_boot_test = list(field = "boot_test", label = "Bootstrap Test Table (.csv)"),
        download_random_walk = list(field = "random_walk", label = "Random Walk Table (.csv)")
      )

      # Get available buttons
      available <- Filter(function(button) !is.null(state[[button$field]]), buttons)

      # Check for no results
      if(length(available) == 0){

        # Show note
        return(tags$p(tags$em("Nothing to download yet. Results appear here once they finish.")))

      }

      # Create buttons
      downloads <- lapply(names(available), function(id){

        # Add button and spacing
        tagList(downloadButton(ns(id), available[[id]]$label), br(), br())

      })

      # Show buttons
      tagList(
        downloadButton(ns("download_all"), "All Results (.rds)"), br(), br(),
        downloads,
        actionButton(ns("reset"), label = "Clear This Session's Results", class = "btn-danger")
      )

    })

    # Handle download of all results
    output$download_all <- downloadHandler(
      filename = function() "semnet_results.rds",
      content = function(file){

        # Save all results
        saveRDS(reactiveValuesToList(state), file)

      }
    )

    # Handle download of networks
    output$download_networks <- downloadHandler(
      filename = function() "semnet_networks.rds",
      content = function(file){

        # Require networks
        req(state$networks)

        # Save networks
        saveRDS(state$networks, file)

      }
    )

    # Handle download of measures
    output$download_measures <- downloadHandler(
      filename = function() "semnet_measures.csv",
      content = function(file){

        # Require measures
        req(state$measures)

        # Save measures
        utils::write.csv(state$measures, file)

      }
    )

    # Handle download of bootstrap tests
    output$download_boot_test <- downloadHandler(
      filename = function() "semnet_bootstrap_test.csv",
      content = function(file){

        # Require tests
        req(state$boot_test)

        # Combine tables of conditions
        table <- do.call(rbind, lapply(names(state$boot_test$results), function(condition){

          # Combine tables of measures
          do.call(rbind, lapply(names(state$boot_test$results[[condition]]), function(measure){

            # Get entry
            entry <- state$boot_test$results[[condition]][[measure]]

            # Add condition and measure
            cbind(Condition = condition, Measure = measure, entry$table)

          }))

        }))

        # Save table
        utils::write.csv(table, file, row.names = FALSE)

      }
    )

    # Handle download of random walk
    output$download_random_walk <- downloadHandler(
      filename = function() "semnet_random_walk.csv",
      content = function(file){

        # Require random walk
        req(state$random_walk)

        # Save table
        utils::write.csv(state$random_walk$short, file, row.names = FALSE)

      }
    )

    # Watch for reset button
    observeEvent(input$reset, {

      # Send confirmation alert
      shinyalert::shinyalert(
        title = "Clear this session's results?",
        text = "This can't be undone. Your uploaded data stays, but every analysis result is cleared.",
        type = "warning", showCancelButton = TRUE, confirmButtonText = "Clear",
        callbackR = function(confirmed){

          # Check for confirmation
          if(isTRUE(confirmed)){

            # Loop over results
            for(field in c("networks", "measures", "random_test", "boot_result", "boot_test", "perm_result", "random_walk", "spreading_result")){

              # Clear result
              state[[field]] <- NULL

            }

          }

        }
      )

    })

  })

}
