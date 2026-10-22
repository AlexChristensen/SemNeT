# Load data module ----
# Updated 22.10.2026

# Load data interface ----
# Updated 22.10.2026
mod_load_data_ui <- function(id)
{

  # Set namespace
  ns <- NS(id)

  # Return interface
  tagList(

    # Add sidebar
    sidebarPanel(

      # Add response matrix upload
      fileInput(
        ns("data"), label = "Upload Response Matrix (raw word responses or a binary matrix)",
        accept = c(".rds", ".csv", ".xls", ".xlsx", ".txt")
      ),

      # Add group upload
      fileInput(ns("group"), label = "Upload Group Variable"),

      # Add response matrix example button
      actionButton(ns("data_example"), label = "See Response Matrix Example"),

      # Add group example button
      actionButton(ns("group_example"), label = "See Group Variable Example"),

      # Add spacing
      br(), br(),

      # Add seed
      numericInput(
        ns("seed"), "Random Seed (0 for no seed)", value = 0, min = 0, max = .Machine$integer.max, step = 1
      ),

      # Add seed note
      tags$p(
        tags$em(
          "Used by every analysis that involves randomness (modularity, random networks, bootstrap, permutation, random walks).",
          "A nonzero seed makes results reproducible; 0 gives different results each run."
        )
      ),

      # Add seed error
      uiOutput(ns("seed_error")),

      # Add load button
      actionButton(ns("load_data"), label = "Load Data", class = "btn-primary"),

      # Add divider
      tags$hr(),

      # Add {SemNetCleaner} picker
      uiOutput(ns("textcleaner_picker"))

    ),

    # Add main panel
    mainPanel(

      # Add logo
      tags$div(
        style = "text-align: center;",
        tags$img(src = "SemNeT.png", width = "500")
      ),

      # Add response matrix example
      tableOutput(ns("example_data_response")),

      # Add binary matrix example
      tableOutput(ns("example_data_binary")),

      # Add group example
      tableOutput(ns("example_group"))

    ),

    # Add citation footer
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

# Load data server ----
# Updated 22.10.2026
mod_load_data_server <- function(id, state)
{

  # Start module
  moduleServer(id, function(input, output, session){

    # Watch for seed
    observeEvent(input$seed, {

      # Store seed (NULL if none or invalid)
      state$seed <- analysis_seed(input$seed)

    }, ignoreNULL = FALSE)

    # Render seed error
    output$seed_error <- renderUI({

      # Check for invalid seed
      if(!valid_seed(input$seed)){

        # Show error
        tags$p(style = "color: #a94442;", "Seed must be a whole number from 0 to 2147483647. No seed is being used.")

      }

    })

    # Find {SemNetCleaner} objects
    textcleaner_objects <- reactiveVal(find_textcleaner_objects())

    # Render {SemNetCleaner} picker
    output$textcleaner_picker <- renderUI({

      # Get objects
      objects <- textcleaner_objects()

      # Get namespace
      ns <- session$ns

      # Check for no objects
      if(length(objects) == 0){

        # Show note and refresh button
        return(tagList(
          tags$p(tags$em(
            "No textcleaner objects (from SemNetCleaner's textcleaner_shiny()) found in your R environment."
          )),
          actionButton(ns("refresh_textcleaner"), "Refresh")
        ))

      }

      # Show picker and buttons
      tagList(
        selectInput(ns("textcleaner_object"), "Or load a textcleaner object from your R environment", choices = objects),
        actionButton(ns("load_textcleaner"), "Load from R Environment", class = "btn-primary"),
        actionButton(ns("refresh_textcleaner"), "Refresh")
      )

    })

    # Watch for refresh
    observeEvent(input$refresh_textcleaner, {

      # Find objects again
      textcleaner_objects(find_textcleaner_objects())

    })

    # Watch for load from environment
    observeEvent(input$load_textcleaner, {

      # Require selected object
      req(input$textcleaner_object)

      # Show notification
      showNotification("Loading data...")

      # Get object
      obj <- tryCatch(get(input$textcleaner_object, envir = .GlobalEnv), error = function(e) NULL)

      # Check for unusable object
      if(is.null(obj) || !inherits(obj, "textcleaner") || !inherits(obj, "fluency")){

        # Send alert
        shinyalert::shinyalert(
          title = "Couldn't load that object",
          text = "It's no longer a fluency-type textcleaner object in your R environment.",
          type = "error"
        )

        # Find objects again
        textcleaner_objects(find_textcleaner_objects())

        # Return nothing
        return(invisible(NULL))

      }

      # Get cleaned responses
      data <- obj$responses$clean

      # Store data
      state$data <- data

      # Store single group
      state$group <- rep(1, nrow(data))

      # Send success alert
      shinyalert::shinyalert(
        title = "Data Loaded Successfully",
        text = paste0("Loaded from `", input$textcleaner_object, "`."),
        type = "success"
      )

      # Check for many cases
      if(nrow(data) > 1000){

        # Send warning alert
        shinyalert::shinyalert(
          title = "There are over 1000 cases in the data",
          text = "Analyses may take considerable time and memory.",
          type = "warning"
        )

      }

    })

    # Watch for response matrix example
    observeEvent(input$data_example, {

      # Show example responses
      output$example_data_response <- renderTable(
        utils::head(SemNeT::open.clean), rownames = TRUE,
        caption = "Response Matrix (participants by typed responses)",
        caption.placement = "top"
      )

      # Show example binary matrix
      output$example_data_binary <- renderTable(
        utils::head(SemNeT::open.binary), rownames = TRUE, digits = 0,
        caption = "Binary Matrix (participants by alphabetical responses)",
        caption.placement = "top"
      )

    })

    # Watch for group example
    observeEvent(input$group_example, {

      # Show example group
      output$example_group <- renderTable({

        # Get example group
        group_example <- as.matrix(utils::head(SemNeT::open.group))

        # Name rows
        row.names(group_example) <- row.names(utils::head(SemNeT::open.binary))

        # Name column
        colnames(group_example) <- "Group"

        # Return example
        group_example

      }, rownames = TRUE, caption = "Group Vector (ordered by participant)", caption.placement = "top")

    })

    # Watch for load data
    observeEvent(input$load_data, {

      # Show notification
      showNotification("Loading data...")

      # Get response matrix
      data <- if(!is.null(input$data)){

        # Read uploaded file
        tryCatch(
          suppressWarnings(read_uploaded_file(input$data$datapath, input$data$name)),
          error = function(e){

            # Send error alert
            shinyalert::shinyalert(title = "Couldn't read the uploaded response matrix", text = conditionMessage(e), type = "error")

            # Return nothing
            NULL

          }
        )

      }else{

        # Use example responses
        SemNeT::open.clean

      }

      # Check for no data
      if(is.null(data)){

        # Return nothing
        return(invisible(NULL))

      }

      # Get group
      group <- if(!is.null(input$group)){

        # Read uploaded file
        tryCatch(
          unlist(suppressWarnings(read_uploaded_file(input$group$datapath, input$group$name))),
          error = function(e){

            # Send error alert
            shinyalert::shinyalert(title = "Couldn't read the uploaded group variable", text = conditionMessage(e), type = "error")

            # Return nothing
            NULL

          }
        )

      }else if(is.null(input$data)){

        # Use example group
        unlist(SemNeT::open.group)

      }else{

        # Use single group
        rep(1, nrow(data))

      }

      # Check for no group
      if(is.null(group)){

        # Return nothing
        return(invisible(NULL))

      }

      # Check for mismatched lengths
      if(length(group) != nrow(data)){

        # Send error alert
        shinyalert::shinyalert(
          title = "Group length doesn't match the response matrix",
          text = paste0("The group variable has ", length(group), " entries, but the response matrix has ", nrow(data), " rows."),
          type = "error"
        )

        # Return nothing
        return(invisible(NULL))

      }

      # Store data
      state$data <- data

      # Store group
      state$group <- group

      # Send success alert
      shinyalert::shinyalert(title = "Data Loaded Successfully", type = "success")

      # Check for many cases
      if(nrow(data) > 1000){

        # Send warning alert
        shinyalert::shinyalert(
          title = "There are over 1000 cases in the data",
          text = "Analyses may take considerable time and memory.",
          type = "warning"
        )

      }

    })

  })

}
