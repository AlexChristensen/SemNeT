# Top-level UI: one `navbarPage` tab per module. Tabs beyond "Load Data"
# start hidden and are revealed by `server.R` as `state` fills in -- see
# `server.R`'s tab-visibility block.

ui <- navbarPage(
  title = "Semantic Network Analysis with SemNeT",
  id = "tabs",

  # `header` (not a bare extra argument) is `navbarPage()`'s documented spot
  # for non-tab content -- `shinyjs::useShinyjs()` just injects a <script>
  # tag and isn't a `tabPanel()`/`navbarMenu()`, so passing it as a plain
  # trailing argument (as this used to) triggered "Navigation containers
  # expect a collection of tabPanel()s...". `shinyalert::useShinyalert()` is
  # dropped entirely -- recent `shinyalert` versions no longer need it (its
  # own package startup message says so directly); every `shinyalert::
  # shinyalert()` call elsewhere in the app is unaffected
  header = shinyjs::useShinyjs(),

  tabPanel("Load Data", mod_load_data_ui("load_data")),
  tabPanel("Network Estimation", mod_network_estimation_ui("network")),
  tabPanel("Random Network Analyses", mod_random_network_ui("random_network")),
  tabPanel("Bootstrap Analyses", mod_bootstrap_ui("bootstrap")),
  tabPanel("Permutation Analyses", mod_permutation_ui("permutation")),
  tabPanel("Random Walk Analyses", mod_random_walk_ui("random_walk")),
  tabPanel("Spreading Activation Analyses", mod_spreading_activation_ui("spreading")),
  tabPanel("Export", mod_export_ui("export"))

)
