# Load Data tab: file upload (or bundled example data) into `state$data`/
# `state$group`. The only analysis-adjacent call here is `unlist()`ing the
# group vector -- everything else is I/O, not a SemNeT algorithm.

mod_load_data_ui <- function(id)
{

  ns <- NS(id)

  tagList(

    sidebarPanel(

      fileInput(
        ns("data"), label = "Upload Response Matrix (raw word responses or a binary matrix)",
        accept = c(".rds", ".csv", ".xls", ".xlsx", ".txt")
      ),

      fileInput(ns("group"), label = "Upload Group Variable"),

      actionButton(ns("data_example"), label = "See Response Matrix Example"),
      actionButton(ns("group_example"), label = "See Group Variable Example"),

      br(), br(),

      actionButton(ns("load_data"), label = "Load Data", class = "btn-primary")

    ),

    mainPanel(

      tags$div(
        style = "text-align: center;",
        tags$img(src = "SemNeT.png", width = "500")
      ),

      tableOutput(ns("example_data_response")),
      tableOutput(ns("example_data_binary")),
      tableOutput(ns("example_group"))

    ),

    tags$footer(
      HTML(paste0(
        "<b>Please cite:</b><br>",
        "Christensen, A. P., & Kenett, Y. N. (2019). Semantic network analysis (SemNA): ",
        "A tutorial on preprocessing, estimating, and analyzing semantic networks. ",
        "<em>PsyArXiv</em>. ",
        "<a href=\"https://doi.org/10.31234/osf.io/eht87\" target=\"_blank\">https://doi.org/10.31234/osf.io/eht87</a>"
      )),
      style = "margin: 20px;"
    )

  )

}

mod_load_data_server <- function(id, state)
{

  moduleServer(id, function(input, output, session){

    observeEvent(input$data_example, {

      output$example_data_response <- renderTable(
        utils::head(SemNeT::open.clean), rownames = TRUE,
        caption = "Response Matrix (participants by typed responses)",
        caption.placement = "top"
      )

      output$example_data_binary <- renderTable(
        utils::head(SemNeT::open.binary), rownames = TRUE, digits = 0,
        caption = "Binary Matrix (participants by alphabetical responses)",
        caption.placement = "top"
      )

    })

    observeEvent(input$group_example, {

      output$example_group <- renderTable({
        group_example <- as.matrix(utils::head(SemNeT::open.group))
        row.names(group_example) <- row.names(utils::head(SemNeT::open.binary))
        colnames(group_example) <- "Group"
        group_example
      }, rownames = TRUE, caption = "Group Vector (ordered by participant)", caption.placement = "top")

    })

    observeEvent(input$load_data, {

      showNotification("Loading data...")

      data <- if(!is.null(input$data)){
        tryCatch(
          suppressWarnings(read_uploaded_file(input$data$datapath, input$data$name)),
          error = function(e){
            shinyalert::shinyalert(title = "Couldn't read the uploaded response matrix", text = conditionMessage(e), type = "error")
            NULL
          }
        )
      }else{
        SemNeT::open.clean
      }

      if(is.null(data)){
        return(invisible(NULL))
      }

      group <- if(!is.null(input$group)){
        tryCatch(
          unlist(suppressWarnings(read_uploaded_file(input$group$datapath, input$group$name))),
          error = function(e){
            shinyalert::shinyalert(title = "Couldn't read the uploaded group variable", text = conditionMessage(e), type = "error")
            NULL
          }
        )
      }else if(is.null(input$data)){
        # Only fall back to the bundled example group when the response
        # matrix is *also* the bundled example -- an uploaded response
        # matrix with no group upload is one group, not the example's groups
        unlist(SemNeT::open.group)
      }else{
        rep(1, nrow(data))
      }

      if(is.null(group)){
        return(invisible(NULL))
      }

      if(length(group) != nrow(data)){
        shinyalert::shinyalert(
          title = "Group length doesn't match the response matrix",
          text = paste0("The group variable has ", length(group), " entries, but the response matrix has ", nrow(data), " rows."),
          type = "error"
        )
        return(invisible(NULL))
      }

      state$data <- data
      state$group <- group

      shinyalert::shinyalert(title = "Data Loaded Successfully", type = "success")

      if(nrow(data) > 1000){
        shinyalert::shinyalert(
          title = "There are over 1000 cases in the data",
          text = "Analyses may take considerable time and memory.",
          type = "warning"
        )
      }

    })

  })

}
