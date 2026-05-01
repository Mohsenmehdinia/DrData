# ============================================================
# DrData — ui.R (full — 10 modules)
# ============================================================

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "DrData"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Import",      tabName = "data",             icon = icon("upload")),
      menuItem("Preprocessing",    tabName = "preprocess",       icon = icon("wrench")),
      menuItem("EDA",              tabName = "eda",              icon = icon("chart-bar")),
      menuItem("Classification",   tabName = "classification",   icon = icon("tags")),
      menuItem("Regression",       tabName = "regression",       icon = icon("chart-line")),
      menuItem("Clustering",       tabName = "clustering",       icon = icon("object-group")),
      menuItem("AutoML",           tabName = "automl",           icon = icon("robot")),
      menuItem("Model Comparison", tabName = "model_comparison", icon = icon("balance-scale")),
      menuItem("Explainability",   tabName = "explainability",   icon = icon("brain")),
      menuItem("Deployment",       tabName = "deployment",       icon = icon("cloud-upload-alt"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "data",             dataUI("data")),
      tabItem(tabName = "preprocess",       preprocessUI("preprocess")),
      tabItem(tabName = "eda",              edaUI("eda")),
      tabItem(tabName = "classification",   classificationUI("classification")),
      tabItem(tabName = "regression",       regressionUI("regression")),
      tabItem(tabName = "clustering",       clusteringUI("clustering")),
      tabItem(tabName = "automl",           automlUI("automl")),
      tabItem(tabName = "model_comparison", modelComparisonUI("model_comparison")),
      tabItem(tabName = "explainability",   explainabilityUI("explainability")),
      tabItem(tabName = "deployment",       deploymentUI("deployment"))
    )
  )
)
