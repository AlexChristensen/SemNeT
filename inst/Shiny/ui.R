# User interface ----
# Updated 13.10.2026
ui <- navbarPage(

  # Set title
  title = "Semantic Network Analysis with SemNeT",

  # Set tab identifier
  id = "tabs",

  # Add {shinyjs} to header
  header = shinyjs::useShinyjs(),

  # Add load data tab
  tabPanel("Load Data", mod_load_data_ui("load_data")),

  # Add network estimation tab
  tabPanel("Network Estimation", mod_network_estimation_ui("network")),

  # Add random network tab
  tabPanel("Random Network Analyses", mod_random_network_ui("random_network")),

  # Add bootstrap tab
  tabPanel("Bootstrap Analyses", mod_bootstrap_ui("bootstrap")),

  # Add permutation tab
  tabPanel("Permutation Analyses", mod_permutation_ui("permutation")),

  # Add random walk tab
  tabPanel("Random Walk Analyses", mod_random_walk_ui("random_walk")),

  # Add spreading activation tab
  tabPanel("Spreading Activation Analyses", mod_spreading_activation_ui("spreading")),

  # Add export tab
  tabPanel("Export", mod_export_ui("export"))

)
