# Network estimation module ----
# Updated 13.10.2026

# Network estimation interface ----
# Updated 13.10.2026
mod_network_estimation_ui <- function(id)
{

  # Set namespace
  ns <- NS(id)

  # Return interface
  tagList(

    # Add sidebar
    sidebarPanel(

      # Add method selection
      selectInput(
        ns("method"), "Network Estimation Method",
        choices = c(
          "Triangulated Maximally Filtered Graph (TMFG)" = "tmfg",
          "Community Network (CN)" = "cn",
          "Naive Random Walk (NRW)" = "nrw",
          "Pathfinder Network (PF)" = "pf"
        )
      ),

      # Add options of method
      uiOutput(ns("method_options")),

      # Add estimate button
      actionButton(ns("run_est"), label = "Estimate Networks", class = "btn-primary")

    ),

    # Add main panel
    mainPanel(

      # Add network plot
      plotOutput(ns("viz")),

      # Add measures table
      tableOutput(ns("measures"))

    ),

    # Add citation footer
    tags$footer(uiOutput(ns("citation")), style = "margin: 20px;")

  )

}

# Network estimation server ----
# Updated 13.10.2026
mod_network_estimation_server <- function(id, state)
{

  # Start module
  moduleServer(id, function(input, output, session){

    # Render options of method
    output$method_options <- renderUI({

      # Get namespace
      ns <- session$ns

      # Show options of method
      switch(
        input$method,

        cn = tagList(
          numericInput(ns("window"), "Window Size", value = 2, min = 1),
          selectInput(ns("alpha"), "Significance Level (two-sided interval; not adjusted for the number of pairs)", choices = c(.05, .01, .001)),
          selectInput(ns("enrich"), "Enrich Network", choices = c(FALSE, TRUE)),
          numericInput(ns("cutoff"), "Enrichment Module Cutoff (no established value; data-dependent)", value = 0.50, min = 0.05, max = 0.95, step = 0.05)
        ),

        nrw = tagList(
          selectInput(ns("nrw_type"), "Minimum Co-Occurrences As", choices = c("Number" = "num", "Proportion" = "prop")),
          numericInput(ns("threshold"), "Threshold", value = 0, min = 0)
        ),

        tmfg = tagList(
          selectInput(
            ns("assoc"), "Association Measure",
            choices = c(
              "Pearson's Correlation" = "cor", "Cosine" = "cosine", "Angular" = "angular",
              "Euclidean Distance (as 1 / (1 + d))" = "euclid", "Faith" = "faith", "Jaccard Index" = "jaccard",
              "Phi Coefficient" = "phi", "RR" = "rr"
            ),
            selected = "cor"
          ),
          numericInput(ns("minimum_cases"), "Minimum Number of Responses", value = 2, min = 1)
        ),

        pf = NULL

      )

    })

    # Watch for estimate button
    observeEvent(input$run_est, {

      # Require data and group
      req(state$data, state$group)

      # Show notification
      showNotification("Estimating networks...")

      # Split data by group
      groups <- split(as.data.frame(state$data), state$group)

      # Estimate networks
      networks <- tryCatch({

        # Estimate networks of method
        switch(
          input$method,

          cn = lapply(groups, cn, window = input$window, alpha = as.numeric(input$alpha), enrich = as.logical(input$enrich), cutoff = input$cutoff),
          nrw = lapply(groups, nrw, type = input$nrw_type, threshold = input$threshold),
          pf = lapply(groups, pf),

          tmfg = {

            # Convert raw responses to binary
            binary_groups <- if(is.character(unlist(state$data))){

              # Convert each group
              lapply(groups, function(group_data) responses2binary(group_data)$binary)

            }else{

              # Use binary groups
              groups

            }

            # Remove rare responses
            finalized <- lapply(binary_groups, finalize, minimum_cases = input$minimum_cases)

            # Equate groups
            equated <- if(length(finalized) > 1){

              # Equate multiple groups
              do.call(equate, finalized)

            }else{

              # Use single group
              finalized

            }

            # Compute similarities
            association <- lapply(equated, similarity, method = input$assoc)

            # Estimate TMFG networks
            lapply(association, tmfg)

          }

        )

      }, error = function(e){

        # Send error alert
        shinyalert::shinyalert(title = "Network estimation failed", text = conditionMessage(e), type = "error")

        # Return nothing
        NULL

      })

      # Check for failed estimation
      if(is.null(networks)){

        # Return nothing
        return(invisible(NULL))

      }

      # Compute measures
      measures <- vapply(networks, semantic_network_measures, numeric(3))

      # Render measures
      output$measures <- renderTable(
        measures, rownames = TRUE, caption = paste(
          "Network Measures (unweighted; clustering coefficient counts nodes with fewer than two neighbors as 0;",
          "modularity (q) is a randomized Louvain, so it can differ slightly between runs)"
        ), caption.placement = "top"
      )

      # Render networks
      output$viz <- renderPlot({

        # Plot networks
        do.call(compare_networks, c(networks, list(config = "spring")))

      })

      # Store groups
      state$groups_data <- groups

      # Store method arguments
      state$method_args <- switch(
        input$method,
        cn = list(window = input$window, alpha = as.numeric(input$alpha), enrich = as.logical(input$enrich), cutoff = input$cutoff),
        nrw = list(type = input$nrw_type, threshold = input$threshold),
        pf = list(),
        tmfg = list()
      )

      # Store similarity
      state$sim <- if(input$method == "tmfg") input$assoc else "cosine"

      # Store minimum cases
      state$minimum_cases <- if(input$method == "tmfg") input$minimum_cases else 2

      # Render citation
      output$citation <- renderUI({

        # Show citation of method
        HTML(switch(
          input$method,
          cn = "<b>Please cite:</b><br>Goni, J., Arrondo, G., Sepulcre, J., Martincorena, I., de Mendizabal, N. V., Corominas-Murtra, B., ... & Villoslada, P. (2011). The semantic organization of the animal category: Evidence from semantic verbal fluency and network theory. <em>Cognitive Processing</em>, <em>12</em>, 183–196.",
          nrw = "<b>Please cite:</b><br>Lerner, A. J., Ogrocki, P. K., & Thomas, P. J. (2009). Network graph analysis of category fluency testing. <em>Cognitive and Behavioral Neurology</em>, <em>22</em>, 45–52.",
          pf = "<b>Please cite:</b><br>Paulsen, J. S., Romero, R., Chan, A., Davis, A. V., Heaton, R. K., & Jeste, D. V. (1996). Impairment of the semantic network in schizophrenia. <em>Psychiatry Research</em>, <em>63</em>, 109–121.",
          tmfg = "<b>Please cite:</b><br>Massara, G. P., Di Matteo, T., & Aste, T. (2016). Network filtering for big data: Triangulated maximally filtered graph. <em>Journal of Complex Networks</em>, <em>5</em>, 161–178."
        ))

      })

      # Store method
      state$method <- input$method

      # Store networks
      state$networks <- networks

      # Store measures
      state$measures <- measures

    })

  })

}
