# Top-level UI: one `navbarPage` tab per module. Tabs beyond "Load Data"
# start hidden and are revealed by `server.R` as `state` fills in -- see
# `server.R`'s tab-visibility block.

ui <- navbarPage(
  title = "Semantic Network Analysis with SemNeT",
  id = "tabs",

  tabPanel("Load Data", mod_load_data_ui("load_data")),
  tabPanel("Network Estimation", mod_network_estimation_ui("network")),
  tabPanel("Random Network Analyses", mod_random_network_ui("random_network")),
  tabPanel("Bootstrap Analyses", mod_bootstrap_ui("bootstrap")),
  tabPanel("Permutation Analyses", mod_permutation_ui("permutation")),
  tabPanel("Random Walk Analyses", mod_random_walk_ui("random_walk")),
  tabPanel("Spreading Activation Analyses", mod_spreading_activation_ui("spreading")),
  tabPanel("Export", mod_export_ui("export")),

  shinyalert::useShinyalert(),
  shinyjs::useShinyjs()

)
