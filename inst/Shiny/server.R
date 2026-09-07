# Top-level server: one `reactiveValues()` per session (`state`) replaces
# every `<<-`/`globalenv()` write the original app made -- nothing here is
# shared across sessions, so concurrent users never clobber each other's
# data (see SHINY_PLAN.md).

server <- function(input, output, session)
{

  # Every module reads/writes this. Fields are filled in as the user moves
  # through the tabs; a tab's own visibility (below) tracks which fields are
  # already populated
  state <- reactiveValues(
    data = NULL,             # response matrix (raw word responses or binary)
    group = NULL,            # group label per row of 'data'
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

  # Every tab past "Load Data" starts hidden, exactly like the original --
  # revealed here (not per-module) since `showTab()`/`hideTab()` act on the
  # top-level `navbarPage`, outside any module's own namespace
  for(later_tab in c(
    "Network Estimation", "Random Network Analyses", "Bootstrap Analyses",
    "Permutation Analyses", "Random Walk Analyses",
    "Spreading Activation Analyses", "Export"
  )){
    hideTab(inputId = "tabs", target = later_tab)
  }

  observeEvent(state$data, {
    if(!is.null(state$data)){
      showTab(inputId = "tabs", target = "Network Estimation")
      updateTabsetPanel(session, "tabs", selected = "Network Estimation")
    }
  })

  observeEvent(state$networks, {
    if(!is.null(state$networks)){
      showTab(inputId = "tabs", target = "Random Network Analyses")
      showTab(inputId = "tabs", target = "Bootstrap Analyses")
      showTab(inputId = "tabs", target = "Permutation Analyses")
      showTab(inputId = "tabs", target = "Random Walk Analyses")
      showTab(inputId = "tabs", target = "Spreading Activation Analyses")
      showTab(inputId = "tabs", target = "Export")
    }
  })

  mod_load_data_server("load_data", state)
  mod_network_estimation_server("network", state)
  mod_random_network_server("random_network", state)
  mod_bootstrap_server("bootstrap", state)
  mod_permutation_server("permutation", state)
  mod_random_walk_server("random_walk", state)
  mod_spreading_activation_server("spreading", state)
  mod_export_server("export", state)

  # A session's own background analyses (bootstrap/permutation/random-network
  # tests, all run via `shiny::ExtendedTask`) are tied to this session's
  # reactive graph and are torn down automatically when it ends -- explicit
  # here only so that's documented, not left implicit
  session$onSessionEnded(function(){
    NULL
  })

}
