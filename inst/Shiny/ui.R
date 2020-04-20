library(shiny)

# Interface for SemNeT
ui <- (
  
  navbarPage(title = "Semantic Network Analysis with SemNeT",
             
             # Network Estimation Panel
             tabPanel(
               "Network Estimation",
               
               # Input
               sidebarPanel(
                 
                 # Environment objects
                 uiOutput("clean_ui"), uiOutput("group_ui"),
                 
                 # Data upload
                 tags$div(fileInput("data", label = "Upload Preprocessed (Binary) Response Matrix",
                                    accept = c(".rds", ".csv", ".xls", ".xlsx", ".txt")), id = "data"),
                 
                 # Group variable upload
                 tags$div(fileInput("group", label = "Upload Group Variable"), id = "group"),
                 
                 # Network estimation method
                 selectInput("estimation", "Network Estimation Method", c("",
                                                                          "Community Network (CN)",
                                                                          "Naive Random Walk (NRW)",
                                                                          "Pathfinder Network (PN)",
                                                                          "Triangulated Maximally Filtered Graph (TMFG)"
                 )),
                 
                 
                 uiOutput("network_options_1"),
                 
                 uiOutput("network_options_2"),
                 
                 actionButton("run_est", label = "Estimate Networks"),
                 
                 actionButton("reset", label = "Clear Results")
                 
               ),
               
               # Output
               mainPanel(
                 plotOutput("viz"),
                 tableOutput("measures")
               )
             ),
             
             # Random Network Analyses Panel
             tabPanel(
               "Random Network Analyses",
               
               # Input
               sidebarPanel(
                 
                 numericInput("iters_rand", label = "Number of Iterations", value = 1000, min = 1),
                 
                 uiOutput("cores_rand"),
                 
                 actionButton("run_rand", label = "Run Random Network Analyses")
                 
               ),
               
               # Output
               mainPanel(
                 tableOutput("randnet")
               )
             ),
             
             # Bootstrap Network Analyses Panel
             tabPanel(
               "Bootstrap Network Analyses",
               
               # Input
               sidebarPanel(
                 
                 numericInput("iters_boot", label = "Number of Iterations", value = 1000, min = 1),
                 
                 uiOutput("cores_boot"),
                 
                 uiOutput("type"),
                 
                 actionButton("run_boot", label = "Run Bootstrap Analyses")
                 
               ),
               
               # Output
               mainPanel(
                 tableOutput("aspl"),
                 tableOutput("cc"),
                 tableOutput("q"),
                 tableOutput("tab")
               )
             ),
             
             # Plot Bootstrap Analyses Panel
             tabPanel(
               "Plot Bootstrap Analyses",
               
               # Input
               sidebarPanel(
                 
                 actionButton("run_plot", label = "Generate Plots")
                 
               ),
               
               # Output
               mainPanel(
                 plotOutput("asplPlot"),
                 plotOutput("ccPlot"),
                 plotOutput("qPlot")
               )
             ),
             
             # Use shinyalert
             shinyalert::useShinyalert()
             
  )
)