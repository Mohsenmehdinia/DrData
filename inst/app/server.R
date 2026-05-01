# ============================================================
# DrData — server.R (full — 10 modules)
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    raw_data            = NULL,
    working_data        = NULL,
    col_types           = NULL,
    # Advanced shared state
    best_model          = NULL,
    best_algo           = NULL,
    task                = NULL,
    train_data          = NULL,
    automl_results_list = list()
  )

  dataServer("data",                     rv)
  preprocessServer("preprocess",         rv)
  edaServer("eda",                       rv)
  classificationServer("classification", rv)
  regressionServer("regression",         rv)
  clusteringServer("clustering",         rv)
  automlServer("automl",                 rv)
  modelComparisonServer("model_comparison", rv)
  explainabilityServer("explainability", rv)
  deploymentServer("deployment",         rv)
}
