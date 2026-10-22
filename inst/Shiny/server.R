# Server ----
# Updated 22.10.2026
server <- function(input, output, session)
{

  # Set up state shared by modules
  state <- reactiveValues(
    data = NULL,             # response matrix (raw word responses or binary)
    group = NULL,            # group label per row of 'data'
    seed = NULL,             # seed from Load Data, NULL for none (random results)
    networks = NULL,         # named list of one estimated network per group
    method = NULL,           # "tmfg"/"cn"/"nrw"/"pf", from Network Estimation
    measures = NULL,         # data frame of aspl/cc/q per group's network
    random_test = NULL,      # random_network_test() result
    boot_result = NULL,      # bootstrap_SemNeT() result
    boot_test = NULL,        # bootstrap_test_SemNeT() result
    perm_result = NULL,      # permutation_SemNeT() result
    random_walk = NULL,      # random_walk() result
    spreading_result = NULL  # spreadr::spreadr() result, plus its inputs
  )

  # Loop over tabs after loading data
  for(later_tab in c(
    "Network Estimation", "Random Network Analyses", "Bootstrap Analyses",
    "Permutation Analyses", "Random Walk Analyses",
    "Spreading Activation Analyses", "Export"
  )){

    # Hide tab
    hideTab(inputId = "tabs", target = later_tab)

  }

  # Watch for loaded data
  observeEvent(state$data, {

    # Check for data
    if(!is.null(state$data)){

      # Show network estimation tab
      showTab(inputId = "tabs", target = "Network Estimation")

      # Switch to network estimation tab
      updateTabsetPanel(session, "tabs", selected = "Network Estimation")

    }

  })

  # Watch for estimated networks
  observeEvent(state$networks, {

    # Check for networks
    if(!is.null(state$networks)){

      # Show random network tab
      showTab(inputId = "tabs", target = "Random Network Analyses")

      # Show bootstrap tab
      showTab(inputId = "tabs", target = "Bootstrap Analyses")

      # Show permutation tab
      showTab(inputId = "tabs", target = "Permutation Analyses")

      # Show random walk tab
      showTab(inputId = "tabs", target = "Random Walk Analyses")

      # Show spreading activation tab
      showTab(inputId = "tabs", target = "Spreading Activation Analyses")

      # Show export tab
      showTab(inputId = "tabs", target = "Export")

    }

  })

  # Start load data module
  mod_load_data_server("load_data", state)

  # Start network estimation module
  mod_network_estimation_server("network", state)

  # Start random network module
  mod_random_network_server("random_network", state)

  # Start bootstrap module
  mod_bootstrap_server("bootstrap", state)

  # Start permutation module
  mod_permutation_server("permutation", state)

  # Start random walk module
  mod_random_walk_server("random_walk", state)

  # Start spreading activation module
  mod_spreading_activation_server("spreading", state)

  # Start export module
  mod_export_server("export", state)

  # End session
  session$onSessionEnded(function(){

    # Do nothing
    NULL

  })

}
