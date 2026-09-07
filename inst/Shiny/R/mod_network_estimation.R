# Network Estimation tab: splits `state$data` by `state$group` and calls
# tmfg()/cn()/nrw()/pf(), similarity(), finalize(), equate(), and
# semantic_network_measures() directly, then compare_networks() to plot.

mod_network_estimation_ui <- function(id)
{

  ns <- NS(id)

  tagList(

    sidebarPanel(

      selectInput(
        ns("method"), "Network Estimation Method",
        choices = c(
          "Triangulated Maximally Filtered Graph (TMFG)" = "tmfg",
          "Community Network (CN)" = "cn",
          "Naive Random Walk (NRW)" = "nrw",
          "Pathfinder Network (PF)" = "pf"
        )
      ),

      uiOutput(ns("method_options")),

      actionButton(ns("run_est"), label = "Estimate Networks", class = "btn-primary")

    ),

    mainPanel(
      plotOutput(ns("viz")),
      tableOutput(ns("measures"))
    ),

    tags$footer(uiOutput(ns("citation")), style = "margin: 20px;")

  )

}

mod_network_estimation_server <- function(id, state)
{

  moduleServer(id, function(input, output, session){

    output$method_options <- renderUI({

      ns <- session$ns

      switch(
        input$method,

        cn = tagList(
          numericInput(ns("window"), "Window Size", value = 2, min = 1),
          selectInput(ns("alpha"), "Significance Level", choices = c(.05, .01, .001)),
          selectInput(ns("enrich"), "Enrich Network", choices = c(FALSE, TRUE))
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
              "Euclidean Distance" = "euclid", "Faith" = "faith", "Jaccard Index" = "jaccard",
              "Phi Coefficient" = "phi", "RR" = "rr"
            ),
            selected = "cor"
          ),
          numericInput(ns("minimum_cases"), "Minimum Number of Responses", value = 2, min = 1)
        ),

        pf = NULL

      )

    })

    observeEvent(input$run_est, {

      req(state$data, state$group)
      showNotification("Estimating networks...")

      groups <- split(as.data.frame(state$data), state$group)

      networks <- tryCatch({

        switch(
          input$method,

          cn = lapply(groups, cn, window = input$window, alpha = as.numeric(input$alpha), enrich = as.logical(input$enrich)),
          nrw = lapply(groups, nrw, type = input$nrw_type, threshold = input$threshold),
          pf = lapply(groups, pf),

          tmfg = {

            binary_groups <- if(is.character(unlist(state$data))){
              lapply(groups, function(group_data) responses_to_binary(group_data)$binary)
            }else{
              groups
            }

            finalized <- lapply(binary_groups, finalize, minimum_cases = input$minimum_cases)

            equated <- if(length(finalized) > 1){
              do.call(equate, finalized)
            }else{
              finalized
            }

            association <- lapply(equated, similarity, method = input$assoc)
            lapply(association, tmfg)

          }

        )

      }, error = function(e){
        shinyalert::shinyalert(title = "Network estimation failed", text = conditionMessage(e), type = "error")
        NULL
      })

      if(is.null(networks)){
        return(invisible(NULL))
      }

      measures <- vapply(networks, semantic_network_measures, numeric(3))

      output$measures <- renderTable(
        measures, rownames = TRUE, caption = "Network Measures", caption.placement = "top"
      )

      output$viz <- renderPlot({
        do.call(compare_networks, c(networks, list(config = "spring")))
      })

      # The Bootstrap/Permutation/Random Walk tabs resample from the same raw,
      # per-group data used here -- `bootstrap_SemNeT()`/`permutation_SemNeT()`
      # do their own binary-conversion/finalize/equate internally (for TMFG),
      # so the *un*processed split is what they need, not `networks` itself
      state$groups_data <- groups
      state$method_args <- switch(
        input$method,
        cn = list(window = input$window, alpha = as.numeric(input$alpha), enrich = as.logical(input$enrich)),
        nrw = list(type = input$nrw_type, threshold = input$threshold),
        pf = list(),
        tmfg = list()
      )
      state$sim <- if(input$method == "tmfg") input$assoc else "cosine"
      state$minimum_cases <- if(input$method == "tmfg") input$minimum_cases else 2

      output$citation <- renderUI({
        HTML(switch(
          input$method,
          cn = "<b>Please cite:</b><br>Goni, J., Arrondo, G., Sepulcre, J., Martincorena, I., de Mendizabal, N. V., Corominas-Murtra, B., ... & Villoslada, P. (2011). The semantic organization of the animal category: Evidence from semantic verbal fluency and network theory. <em>Cognitive Processing</em>, <em>12</em>, 183–196.",
          nrw = "<b>Please cite:</b><br>Lerner, A. J., Ogrocki, P. K., & Thomas, P. J. (2009). Network graph analysis of category fluency testing. <em>Cognitive and Behavioral Neurology</em>, <em>22</em>, 45–52.",
          pf = "<b>Please cite:</b><br>Paulsen, J. S., Romero, R., Chan, A., Davis, A. V., Heaton, R. K., & Jeste, D. V. (1996). Impairment of the semantic network in schizophrenia. <em>Psychiatry Research</em>, <em>63</em>, 109–121.",
          tmfg = "<b>Please cite:</b><br>Massara, G. P., Di Matteo, T., & Aste, T. (2016). Network filtering for big data: Triangulated maximally filtered graph. <em>Journal of Complex Networks</em>, <em>5</em>, 161–178."
        ))
      })

      state$method <- input$method
      state$networks <- networks
      state$measures <- measures

    })

  })

}
