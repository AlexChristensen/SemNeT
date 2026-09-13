# Load Data tab: file upload (or bundled example data) into `state$data`/
# `state$group`. The only analysis-adjacent call here is `unlist()`ing the
# group vector -- everything else is I/O, not a SemNeT algorithm.
#
# Also recognizes a `textcleaner`-classed object (from SemNetCleaner's own
# `textcleaner_shiny()` app -- a sibling package) already sitting in the
# user's `.GlobalEnv`, as a second data source alongside the file upload,
# per direct request ("separate from browsing the computer"). Both apps are
# ordinarily launched sequentially from the same interactive R session
# (`shiny::runApp()` blocks the console until stopped), so by the time this
# app starts, anything the user saved via textcleaner_shiny()'s own "Save to
# R Environment" button is already sitting in `.GlobalEnv` -- no file, temp
# path, or `options()` signal needed between the two packages, just the
# shared environment both happen to run in. Restricted to `type = "fluency"`
# objects: a `type = "free"` result's `$data$clean` is a long-format
# response/cue/ID table, a different shape than the participant x
# response-slot word matrix this app's own `responses_to_binary()` (and
# every bundled example dataset) expects -- not something to silently
# reshape without a real design decision, so it's excluded from the
# picker entirely rather than offered and failing downstream.
#
# find_textcleaner_objects() itself lives in R/shiny_helpers.R, not here --
# real, exported, tested package logic, not UI glue, matching this
# package's own precedent for read_uploaded_file().

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

      actionButton(ns("load_data"), label = "Load Data", class = "btn-primary"),

      tags$hr(),

      uiOutput(ns("textcleaner_picker"))

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

    # Computed once at session start (typical use: textcleaner_shiny() and
    # SemNeTShiny() are launched sequentially in the same console, so
    # anything worth finding is already in .GlobalEnv by the time this app
    # starts) and refreshable via its own button, for the less common case
    # of assigning one mid-session.
    textcleaner_objects <- reactiveVal(find_textcleaner_objects())

    output$textcleaner_picker <- renderUI({

      objects <- textcleaner_objects()
      ns <- session$ns

      if(length(objects) == 0){
        return(tagList(
          tags$p(tags$em(
            "No textcleaner objects (from SemNetCleaner's textcleaner_shiny()) found in your R environment."
          )),
          actionButton(ns("refresh_textcleaner"), "Refresh")
        ))
      }

      tagList(
        selectInput(ns("textcleaner_object"), "Or load a textcleaner object from your R environment", choices = objects),
        actionButton(ns("load_textcleaner"), "Load from R Environment", class = "btn-primary"),
        actionButton(ns("refresh_textcleaner"), "Refresh")
      )

    })

    observeEvent(input$refresh_textcleaner, {
      textcleaner_objects(find_textcleaner_objects())
    })

    observeEvent(input$load_textcleaner, {

      req(input$textcleaner_object)

      showNotification("Loading data...")

      obj <- tryCatch(get(input$textcleaner_object, envir = .GlobalEnv), error = function(e) NULL)

      if(is.null(obj) || !inherits(obj, "textcleaner") || !inherits(obj, "fluency")){
        shinyalert::shinyalert(
          title = "Couldn't load that object",
          text = "It's no longer a fluency-type textcleaner object in your R environment.",
          type = "error"
        )
        textcleaner_objects(find_textcleaner_objects())
        return(invisible(NULL))
      }

      data <- obj$responses$clean

      state$data <- data
      state$group <- rep(1, nrow(data))

      shinyalert::shinyalert(
        title = "Data Loaded Successfully",
        text = paste0("Loaded from `", input$textcleaner_object, "`."),
        type = "success"
      )

      if(nrow(data) > 1000){
        shinyalert::shinyalert(
          title = "There are over 1000 cases in the data",
          text = "Analyses may take considerable time and memory.",
          type = "warning"
        )
      }

    })

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
