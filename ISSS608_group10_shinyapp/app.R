library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggstatsplot)
library(ggthemes)
library(ggdist)
library(scales)
library(tidymodels)
library(vip)
library(rpart.plot)
library(DT)


data_root <- "data"

# Load and bin the data for the Association Test
customers <- readRDS("data/customers_shiny.rds")


pred_data <- readRDS("data/predictive_precomputed.rds")
sat_data <- readRDS("data/satisfaction_precomputed.rds")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "COFINFAD Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home Page", tabName = "home", icon = icon("home")),
      
      # 1. Confirmatory Analysis with Sub-tabs
      menuItem("Confirmatory Analysis", icon = icon("microscope"), startExpanded = TRUE,
               menuSubItem("ANOVA Test", tabName = "anova"),
               menuSubItem("Association Analysis", tabName = "association")
      ),
      
      # 2. Clustering Analysis
      menuItem("Clustering Analysis", tabName = "clustering", icon = icon("project-diagram")),
      
      # 3. Predictive Modelling
      menuItem("Predictive Modelling", icon = icon("chart-line"), startExpanded = TRUE,
               menuSubItem("Satisfaction Score", tabName = "predictive_satisfaction"),
               menuSubItem("Transaction Behaviour", tabName = "predictive_transaction")
      ),
      
      hr(),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              h2("Exploratory Data Analysis"),
              h3("Demographic Variables"),

              fluidRow(
                # Control Box (Width 3)
                box(title = "Select Demographic", width = 3, status = "primary", solidHeader = TRUE,
                    selectInput("demo_select", "Choose Variable:", 
                                choices = c("Education Level" = "education_level", 
                                            "Marital Status" = "marital_status", 
                                            "Gender" = "gender", 
                                            "Age Group" = "age_group", 
                                            "Income Bracket" = "income_bracket", 
                                            "Household Size" = "hh_group"))
                ),
                # Pie Chart Box (Width 4)
                box(title = "Horizontal Bar Chart", width = 9, status = "primary",
                    plotOutput("demoPieChart"))

                #box(title = "Half Eye Graph and Boxplot", width = 3, status = "primary",
                 #     plotOutput("demoDistPlot")),
               # box(title = "Half Eye Graph and Boxplot (Log-Transformed Distribution)", width = 3, status = "primary",
                 #     plotOutput("demoLogPlot"))
                ),
              
              h3("Spending Behaviors Distribution"),
              fluidRow(
                box(title = "Select Financial Metric", width = 3, status = "success", solidHeader = TRUE,
                    selectInput("spend_select", "Choose Metric:",
                                choices = c("Transaction Count (Frequency)" = "tx_count",
                                            "Average Transaction Value (COP)" = "avg_tx_value",
                                            "Total Transaction Volume (COP)" = "total_tx_volume"))
                ),
                box(title = "Distribution (Histogram)", width = 3, status = "success",
                    plotOutput("spendHistogram")),
                box(title = "Cumulative Proportion", width = 3, status = "success",
                    plotOutput("spendECDF")),
                box(title = "Outlier Analysis", width = 3, status = "success",
                    plotOutput("spendBoxplot"))
              ),
              h3("Cross-Variable Analysis"),
              fluidRow(
                box(title = "Demographic vs. Financial Breakdown", width = 12, status = "warning",
                    
                    # NEW: A toggle switch for the user!
                    checkboxInput("log_y", "Apply Log-10 Transformation to Y-Axis", value = TRUE),
                    
                    plotOutput("crossBoxplot", height = "500px"))
              )
      ),
      
      # ANOVA Sub-tab
      tabItem(tabName = "anova", 
              fluidRow(
                box(title = "ANOVA Controls", width = 3, status = "info", solidHeader = TRUE,
                    
                    # Dropdown 1: Financial Metric
                    selectInput("anova_financial", "Select Financial Metric:", 
                                choices = c("Transaction Count (Frequency)" = "tx_count",
                                            "Average Transaction Value (COP)" = "avg_tx_value",
                                            "Total Transaction Volume (COP)" = "total_tx_volume"),
                                selected = "avg_tx_value"),
                    
                    # Dropdown 2: Demographic
                    selectInput("anova_var", "Select Demographic:", 
                                choices = c("Education Level" = "education_level", 
                                            "Marital Status" = "marital_status", 
                                            "Gender" = "gender", 
                                            "Age Group" = "age_group", 
                                            "Income Bracket" = "income_bracket", 
                                            "Household Size" = "hh_group")),
                    
                    # Dropdown 3: Test Type
                    selectInput("anova_type", "Select Test Type:", 
                                choices = c("Parametric" = "parametric", 
                                            "Non-parametric" = "nonparametric", 
                                            "Robust" = "robust", 
                                            "Bayes Factor" = "bayes"),
                                selected = "nonparametric"),
                    
                    # NEW: Log-10 Checkbox for the ANOVA chart
                    checkboxInput("anova_log_y", "Apply Log-10 Transformation to Y-Axis", value = TRUE)
                ),
                # THIS IS THE MISSING PART: The box to display the actual plot, and the closing brackets!
                box(title = "Statistical Results (ggbetweenstats)", width = 9, status = "info",
                    plotOutput("anovaPlot", height = "600px")) 
              )
      ),
      
      tabItem(tabName = "association", 
              fluidRow(
                box(title = "Association Controls", width = 3, status = "info", solidHeader = TRUE,
                    
                    # Dropdown 1: Financial Metric
                    selectInput("assoc_financial", "Select Financial Metric:", 
                                choices = c("Average Transaction Value" = "Average_Transaction_bins",
                                            "Transaction Frequency" = "Transaction_Count_bins",
                                            "Total Transaction Volume" = "Total_Volume_bins")),
                    
                    # Dropdown 2: Demographic
                    selectInput("assoc_var", "Select Demographic:", 
                                choices = c("Education Level" = "education_level", 
                                            "Marital Status" = "marital_status", 
                                            "Gender" = "gender", 
                                            "Age Group" = "age_group", 
                                            "Income Bracket" = "income_bracket", 
                                            "Household Size" = "hh_group")),
                    
                    # NEW Dropdown 3: Test Type
                    selectInput("assoc_type", "Select Test Type:", 
                                choices = c("Parametric (Pearson's Chi-squared)" = "parametric", 
                                            "Non-parametric" = "nonparametric", 
                                            "Robust" = "robust", 
                                            "Bayes Factor" = "bayes"),
                                selected = "parametric")
                ),
                
                box(title = "Test of Association (ggbarstats)", width = 9, status = "info",
                    plotOutput("assocPlot", height = "600px"))
              )
      ),
      
      # Clustering Tab
      tabItem(
        tabName = "clustering",
        h2("Clustering Analysis"),
        
        fluidRow(
          box(
            title = "Clustering Panel",
            width = 3,
            status = "warning",
            solidHeader = TRUE,
            
            selectInput(
              "cluster_vars",
              "Select Variables:",
              choices = c(
                "Age" = "age",
                "Transaction Count" = "tx_count",
                "Avg Transaction Value" = "avg_tx_value",
                "Total Volume" = "total_tx_volume"
              ),
              multiple = TRUE,
              selected = c("age", "avg_tx_value")
            ),
            
            actionButton("clear_vars", "Clear Selection"),
            br(), br(),
            
            sliderInput("num_clusters", "Number of Clusters:", min = 2, max = 6, value = 3),
            sliderInput("num_reps", "Repetitions:", min = 1, max = 10, value = 5),
            
            actionButton("run_cluster", "Run Cluster")
          ),
          
          box(
            title = "Cluster Plot",
            width = 5,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("clusterPlot", height = "350px")
          ),
          
          box(
            title = "Cluster Proportion",
            width = 4,
            status = "success",
            solidHeader = TRUE,
            plotOutput("clusterPropPlot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Cluster Summary",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            tableOutput("clusterSummary")
          )
        )
      ),
      
      
      
      # Predictive Tab
      tabItem(
        tabName = "predictive_satisfaction",
        h2("Predictive Modelling: Satisfaction Score"),
        fluidRow(
          valueBoxOutput("satisfactionBestModelBox", width = 4),
          valueBoxOutput("satisfactionBestRmseBox", width = 4),
          valueBoxOutput("satisfactionTargetBox", width = 4)
        ),
        fluidRow(
          box(
            title = "Predictive Controls",
            width = 3,
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "satisfaction_model",
              "Choose model view:",
              choices = c("Random Forest", "Linear Regression", "Decision Tree"),
              selected = "Random Forest"
            ),
            p("Validation diagnostics are based on the satisfaction-score workflow in Take Home Exercise 2."),
            tableOutput("satisfactionMetrics")
          ),
          box(
            title = "Validation Performance Comparison",
            width = 9,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("satisfactionComparisonPlot", height = "320px")
          )
        ),
        fluidRow(
          box(
            title = "Predicted vs Actual",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("satisfactionActualPlot", height = "320px")
          ),
          box(
            title = "Residual Plot",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("satisfactionResidualPlot", height = "320px")
          )
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.satisfaction_model == 'Random Forest'",
            box(
              title = "Random Forest Feature Importance",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              plotOutput("satisfactionVipPlot", height = "420px")
            )
          ),
          conditionalPanel(
            condition = "input.satisfaction_model == 'Linear Regression'",
            box(
              title = "Linear Regression Coefficients",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              plotOutput("satisfactionCoefPlot", height = "420px")
            )
          ),
          conditionalPanel(
            condition = "input.satisfaction_model == 'Decision Tree'",
            box(
              title = "Decision Tree Structure",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              plotOutput("satisfactionTreePlot", height = "420px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "predictive_transaction",
        h2("Predictive Modelling: Transaction Behaviour"),
        fluidRow(
          valueBoxOutput("bestModelBox", width = 4),
          valueBoxOutput("bestRmseBox", width = 4),
          valueBoxOutput("predictiveTargetBox", width = 4)
        ),
        fluidRow(
          box(
            title = "Predictive Controls",
            width = 3,
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "predictive_model",
              "Choose model view:",
              choices = c("Random Forest", "Linear Regression", "Decision Tree"),
              selected = "Random Forest"
            ),
            p("Validation diagnostics are based on the transaction behaviour workflow in Take Home Exercise 2B."),
            tableOutput("predictiveMetrics")
          ),
          box(
            title = "Validation Performance Comparison",
            width = 9,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("predictiveComparisonPlot", height = "320px")
          )
        ),
        fluidRow(
          box(
            title = "Predicted vs Actual",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("predictiveActualPlot", height = "320px")
          ),
          box(
            title = "Residual Plot",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("predictiveResidualPlot", height = "320px")
          )
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.predictive_model == 'Random Forest'",
            box(
              title = "Random Forest Feature Importance",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              plotOutput("predictiveVipPlot", height = "420px")
            )
          ),
          conditionalPanel(
            condition = "input.predictive_model == 'Linear Regression'",
            box(
              title = "Linear Regression Coefficients",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              plotOutput("predictiveCoefPlot", height = "420px")
            )
          ),
          conditionalPanel(
            condition = "input.predictive_model == 'Decision Tree'",
            box(
              title = "Decision Tree Structure",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              plotOutput("predictiveTreePlot", height = "420px")
            )
          )
        )
      ),
      
      # Data Table Tab
      tabItem(
        tabName = "table",
        h2("Raw Data View"),
        fluidRow(
          box(
            title = "customers_shiny.rds",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("rawDataTable")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  
  
  draw_tree_plot <- function(tree_model, palette = "GnBu") {
    rpart.plot::prp(
      tree_model,
      type = 4,
      extra = 101,
      under = TRUE,
      fallen.leaves = TRUE,
      faclen = 0,
      varlen = 0,
      roundint = FALSE,
      nn = TRUE,
      branch = 0.45,
      branch.lty = 1,
      branch.col = "#6b7280",
      box.palette = palette,
      shadow.col = "#cbd5e1",
      border.col = "#475569",
      split.border.col = "#94a3b8",
      split.box.col = "#e2e8f0",
      split.cex = 1.1,
      tweak = 1.25,
      cex = 0.85,
      main = ""
    )
  }
  
  satisfaction_selected_preds <- reactive({
    req(input$satisfaction_model)
    
    sat_data$all_preds %>%  # Updated to pull from the precomputed list
      filter(model == input$satisfaction_model)
  })
  
  predictive_selected_preds <- reactive({
    req(input$predictive_model)
    
    pred_data$all_preds %>% # Updated to pull from the precomputed list
      filter(model == input$predictive_model)
  })
  
  
  
  
  
  
  # 1. Create a reactive label for the financial metric
  financial_label <- reactive({
    switch(input$spend_select,
           "tx_count" = "Transaction Count",
           "avg_tx_value" = "Average Transaction $",
           "total_tx_volume" = "Total Transaction $")
  })
  
  # 2. Create a reactive label for the demographic metric (Fixes ugly titles!)
  # Right now your titles say "Avg Transaction Value by education_level"
  # This fixes it to say "... by Education Level"
  demo_label <- reactive({
    names(which(c("Education Level" = "education_level", 
                  "Marital Status" = "marital_status", 
                  "Gender" = "gender", 
                  "Age Group" = "age_group", 
                  "Income Bracket" = "income_bracket", 
                  "Household Size" = "hh_group") == input$demo_select))
  })
  
  output$demoPieChart <- renderPlot({
    plot_data <- customers %>%
      group_by(!!sym(input$demo_select)) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    # reorder() automatically sorts the bars from largest to smallest!
    ggplot(plot_data, aes(x = reorder(!!sym(input$demo_select), percentage), 
                          y = percentage, 
                          fill = !!sym(input$demo_select))) +
      geom_col(color = "white", width = 0.7) +
      coord_flip() + # Flips the chart horizontally
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                hjust = -0.2, fontface = "bold", color = "#014d64") +
      
      labs(title = paste("Distribution of", input$demo_select), y = "Percentage (%)") +
      scale_fill_economist() +
      theme_economist() +
      theme(
        legend.position = "none", # Hide legend since the axis already has the names!
        axis.title.y = element_blank(), # Removes the redundant Y axis label
        axis.title.x = element_text(vjust = -1, face = "bold")
      )
  })


output$spendHistogram <- renderPlot({
  
  # Your custom Economist histogram, fully reactive!
  ggplot(data = customers, aes(x = !!sym(input$spend_select))) +
    geom_histogram(bins = 20, boundary = 100, color = "grey25", fill = "grey90") +
    labs(x = financial_label(), 
         title = paste("Distribution of", financial_label())) +
    theme_economist() +
    # Note: 'display="block"' isn't a standard ggplot argument, but I kept your vjust logic!
    theme(axis.title.x = element_text(size = 12, vjust = -1)) 
})

# --- Chart 2: ECDF Plot ---
output$spendECDF <- renderPlot({

  
  ggplot(data = customers, aes(x = !!sym(input$spend_select))) +
    stat_ecdf(geom = "step", color = "#014d64", linewidth = 1) + # Economist blue line
    labs(x = financial_label(), y = "Cumulative Proportion (%)",
         title = paste("Cumulative Trend of", financial_label())) +
    theme_economist() +
    theme(axis.title.x = element_text(size = 12, vjust = -1),
          axis.title.y = element_text(size = 12, vjust = 2))
})

# --- Chart 3: Outlier Boxplot ---
output$spendBoxplot <- renderPlot({

  
  ggplot(data = customers, aes(x = !!sym(input$spend_select), y = "")) +
    geom_boxplot(fill = "grey90", color = "grey25", 
                 outlier.color = "red", outlier.alpha = 0.5) + # Highlights whales in red
    labs(x = financial_label(), y = NULL,
         title = paste("Outliers in", financial_label())) +
    theme_economist() +
    theme(axis.title.x = element_text(size = 12, vjust = -1),
          axis.text.y = element_blank(),  # Hide Y axis text since it's just 1D
          axis.ticks.y = element_blank())
})

output$demoDistPlot <- renderPlot({
  
  # Added 'fill' so the distributions use the Economist color palette
  ggplot(customers, aes(x = !!sym(input$demo_select), 
                        y = avg_tx_value,
                        fill = !!sym(input$demo_select))) +
    stat_halfeye(
      adjust = 0.5,
      justification = -0.2,
      .width = 0,
      point_colour = NA
    ) +
    geom_boxplot(
      width = 0.20,
      outlier.shape = NA,
      alpha = 0.7 # Added slight transparency so it blends nicely
    ) +
    stat_dots(
      side = "left",
      justification = 1.2,
      binwidth = 0.5, 
      dotsize = 2,
      color = NA # Removes black borders from the dots for a cleaner look
    ) +
    
    scale_fill_economist() + # 1. Apply Economist Colors
    
    labs(
      x = input$demo_select,
      y = "Average Transaction Value",
      title = paste("Avg Transaction Value by", input$demo_select)
    ) +
    
    theme_economist() +      # 2. Apply Economist Background/Fonts
    
    # 3. Force titles to appear and hide the redundant legend
    theme(
      axis.title.x = element_text(size = 12, vjust = -1, face = "bold"),
      axis.title.y = element_text(size = 12, vjust = 2, face = "bold"),
      legend.position = "none" 
    )
})

output$demoLogPlot <- renderPlot({
  
  ggplot(customers, aes(x = !!sym(input$demo_select), 
                        y = avg_tx_value, 
                        fill = !!sym(input$demo_select))) +
    stat_halfeye(
      adjust = 0.5,
      justification = -0.2,
      .width = 0,
      point_colour = NA
    ) +
    geom_boxplot(
      width = 0.20,
      outlier.shape = NA,
      alpha = 0.7 # Added transparency to match your other plot
    ) +
    scale_y_log10(
      labels = scales::label_number(scale = 1e-6, suffix = "M"),
      name = "Avg Transaction Value (COP) - Log Scale"
    ) +
    
    scale_fill_economist() + # 1. Apply Economist Colors
    
    labs(
      title = paste("Log-Transformed Distribution by", input$demo_select),
      x = input$demo_select
    ) +
    
    theme_economist() +      # 2. Apply Economist Background/Fonts
    
    # 3. Force titles back and hide the legend
    theme(
      axis.title.x = element_text(size = 12, vjust = -1, face = "bold"),
      axis.title.y = element_text(size = 12, vjust = 2, face = "bold"),
      legend.position = "none" 
    )
})

output$anovaPlot <- renderPlot({
  
  # 1. Create a clean label for the Y-axis and Title
  metric_label <- switch(input$anova_financial,
                         "tx_count" = "Transaction Count",
                         "avg_tx_value" = "Average Transaction Value (COP)",
                         "total_tx_volume" = "Total Transaction Volume (COP)")
  
  # 2. Generate the base ggbetweenstats plot and save it as 'p'
  p <- ggbetweenstats(
    data = customers,
    x = !!sym(input$anova_var), 
    y = !!sym(input$anova_financial), 
    type = input$anova_type, 
    ylab = metric_label,              
    xlab = input$anova_var,
    title = paste("ANOVA Test:", metric_label, "by", input$anova_var),
    mean.ci = TRUE, 
    pairwise.comparisons = TRUE, 
    pairwise.display = "s",
    p.adjust.method = "fdr",
    messages = FALSE
  )
  
  # 3. If the checkbox is checked, add the log scale!
  if (input$anova_log_y) {
    p <- p + 
      scale_y_log10(labels = scales::comma) + 
      labs(y = paste(metric_label, "(Log10 Scale)"))
  }
  
  # 4. Print the final plot
  return(p)
  
})

output$crossBoxplot <- renderPlot({
  
  # 1. Build the base plot and save it as 'p'
  p <- ggplot(customers, aes(x = !!sym(input$demo_select), 
                             y = !!sym(input$spend_select), 
                             fill = !!sym(input$demo_select))) +
    geom_boxplot(outlier.color = "red", outlier.alpha = 0.5) +
    scale_fill_economist() +
    theme_economist() +
    labs(
      title = paste(financial_label(), "by", demo_label()),
      x = demo_label(),
      y = financial_label()
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 12, face = "bold", vjust = -1),
      axis.title.y = element_text(size = 12, face = "bold", vjust = 2)
    )
  
  # 2. If the user checked the box, apply the Log transformation!
  if (input$log_y) {
    p <- p + 
      scale_y_log10(labels = scales::comma) + # Uses comma format instead of scientific notation
      labs(y = paste(financial_label(), "(Log10 Scale)"))
  }
  
  # 3. Print the plot
  return(p)
})

output$assocPlot <- renderPlot({
  
  # Optional: Create clean labels for the title
  fin_label <- switch(input$assoc_financial,
                      "Average_Transaction_bins" = "Average Transaction Value",
                      "Transaction_Count_bins" = "Transaction Frequency",
                      "Total_Volume_bins" = "Total Transaction Volume")
  
  ggbarstats(
    data = customers,
    x = !!sym(input$assoc_financial), 
    y = !!sym(input$assoc_var),       
    type = input$assoc_type,          # NEW: Connects to the test type dropdown
    title = paste("Association:", fin_label, "vs", input$assoc_var),
    messages = FALSE
  )
  
})

observeEvent(input$clear_vars, {
  updateSelectInput(session, "cluster_vars", selected = character(0))
})

cluster_result <- eventReactive(input$run_cluster, {
  
  req(input$cluster_vars)
  req(length(input$cluster_vars) >= 2)
  
  cluster_data <- customers %>%
    select(all_of(input$cluster_vars)) %>%
    drop_na()
  
  scaled_data <- scale(cluster_data)
  
  km <- kmeans(
    scaled_data,
    centers = input$num_clusters,
    nstart = input$num_reps
  )
  
  plot_data <- cluster_data %>%
    mutate(Cluster = factor(km$cluster))
  
  list(data = plot_data)
})

output$clusterPlot <- renderPlot({
  req(cluster_result())
  
  data <- cluster_result()$data
  vars <- input$cluster_vars
  
  ggplot(data, aes_string(x = vars[1], y = vars[2], color = "Cluster")) +
    geom_point(size = 3, alpha = 0.7) +
    stat_ellipse(aes(fill = Cluster), geom = "polygon", alpha = 0.15, color = NA) +
    theme_minimal() +
    labs(
      title = "Cluster Plot",
      x = vars[1],
      y = vars[2]
    )
})

output$clusterPropPlot <- renderPlot({
  req(cluster_result())
  
  cluster_result()$data %>%
    count(Cluster) %>%
    ggplot(aes(x = Cluster, y = n, fill = Cluster)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5) +
    theme_minimal() +
    labs(title = "Cluster Proportion", y = "Count")
})

output$clusterSummary <- renderTable({
  req(cluster_result())
  
  cluster_result()$data %>%
    group_by(Cluster) %>%
    summarise(across(all_of(input$cluster_vars), ~ round(mean(.x, na.rm = TRUE), 2)))
})

# ---------------- PREDICTIVE MODELLING: SATISFACTION ----------------

# Calculate the best models on the fly from your metrics tables
sat_best_model <- sat_data$metrics_table %>% arrange(RMSE) %>% slice(1)
pred_best_model <- pred_data$metrics_table %>% arrange(RMSE) %>% slice(1)

output$satisfactionBestModelBox <- renderValueBox({
  valueBox(
    value = sat_best_model$model,
    subtitle = "Best validation model",
    icon = icon("award"),
    color = "green"
  )
})

output$satisfactionBestRmseBox <- renderValueBox({
  valueBox(
    value = number(sat_best_model$RMSE, accuracy = 0.001),
    subtitle = "Lowest RMSE",
    icon = icon("bullseye"),
    color = "blue"
  )
})

output$satisfactionTargetBox <- renderValueBox({
  valueBox(
    value = "satisfaction_score",
    subtitle = "Prediction target",
    icon = icon("smile"),
    color = "yellow"
  )
})

output$satisfactionMetrics <- renderTable({
  sat_data$metrics_table %>%
    mutate(
      RMSE = round(RMSE, 3),
      MAE = round(MAE, 3),
      R2 = round(R2, 3)
    )
})

output$satisfactionComparisonPlot <- renderPlot({
  ggplot(sat_data$metrics_plot, aes(x = model, y = Value, fill = model)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    facet_wrap(~ Metric, scales = "free_y", nrow = 1) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_discrete(labels = c("Decision\nTree", "Linear\nRegression", "Random\nForest")) +
    labs(
      title = "Model Performance Comparison on Validation Set",
      x = "Model",
      y = "Metric Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 11),
      strip.text = element_text(size = 11, face = "bold")
    )
})

output$satisfactionActualPlot <- renderPlot({
  ggplot(satisfaction_selected_preds(), aes(x = satisfaction_score, y = .pred)) +
    geom_point(alpha = 0.3, color = "#2c7fb8") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(
      title = paste(input$satisfaction_model, ": Predicted vs Actual"),
      x = "Actual Satisfaction Score",
      y = "Predicted Satisfaction Score"
    ) +
    theme_minimal()
})

output$satisfactionResidualPlot <- renderPlot({
  ggplot(satisfaction_selected_preds(), aes(x = .pred, y = residual)) +
    geom_point(alpha = 0.3, color = "#636363") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste(input$satisfaction_model, ": Residual Plot"),
      x = "Predicted Values",
      y = "Residuals"
    ) +
    theme_minimal()
})

output$satisfactionVipPlot <- renderPlot({
  vip(sat_data$rf_vip, num_features = 15) +
    labs(title = "Top 15 Features for Random Forest") +
    theme_minimal()
})

output$satisfactionCoefPlot <- renderPlot({
  ggplot(sat_data$lm_coef, aes(x = reorder(term, abs_estimate), y = estimate, fill = estimate > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
    labs(
      title = "Top 15 Linear Regression Coefficients",
      x = "Predictor",
      y = "Coefficient Estimate"
    ) +
    theme_minimal()
})

output$satisfactionTreePlot <- renderPlot({
  # Load the tree model directly from the file path
  sat_tree <- readRDS(file.path(data_root, "satisfaction", "models", "final_tree.rds"))
  draw_tree_plot(
    extract_fit_engine(sat_tree),
    palette = "Greens"
  )
})

# ---------------- PREDICTIVE MODELLING (TRANSACTIONS) ----------------
output$bestModelBox <- renderValueBox({
  valueBox(
    value = pred_best_model$model,
    subtitle = "Best validation model",
    icon = icon("award"),
    color = "green"
  )
})

output$bestRmseBox <- renderValueBox({
  valueBox(
    value = number(pred_best_model$RMSE, accuracy = 0.001),
    subtitle = "Lowest RMSE",
    icon = icon("bullseye"),
    color = "blue"
  )
})

output$predictiveTargetBox <- renderValueBox({
  valueBox(
    value = "log1p(avg_daily_transactions)",
    subtitle = "Prediction target",
    icon = icon("chart-line"),
    color = "yellow"
  )
})

output$predictiveMetrics <- renderTable({
  pred_data$metrics_table %>%
    mutate(
      RMSE = round(RMSE, 3),
      MAE = round(MAE, 3),
      R2 = round(R2, 3)
    )
})

output$predictiveComparisonPlot <- renderPlot({
  ggplot(pred_data$metrics_plot, aes(x = model, y = Value, fill = model)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    facet_wrap(~ Metric, scales = "free_y", nrow = 1) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_discrete(labels = c("Decision\nTree", "Linear\nRegression", "Random\nForest")) +
    labs(
      title = "Model Performance Comparison on Validation Set",
      x = "Model",
      y = "Metric Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 11),
      strip.text = element_text(size = 11, face = "bold")
    )
})

output$predictiveActualPlot <- renderPlot({
  ggplot(predictive_selected_preds(), aes(x = log_avg_daily_transactions, y = .pred)) +
    geom_point(alpha = 0.3, color = "#2c7fb8") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(
      title = paste(input$predictive_model, ": Predicted vs Actual"),
      x = "Actual Log Avg Daily Transactions",
      y = "Predicted Log Avg Daily Transactions"
    ) +
    theme_minimal()
})

output$predictiveResidualPlot <- renderPlot({
  ggplot(predictive_selected_preds(), aes(x = .pred, y = residual)) +
    geom_point(alpha = 0.3, color = "#636363") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste(input$predictive_model, ": Residual Plot"),
      x = "Predicted Values",
      y = "Residuals"
    ) +
    theme_minimal()
})

output$predictiveVipPlot <- renderPlot({
  vip(pred_data$rf_vip, num_features = 15) +
    labs(title = "Top 15 Features for Random Forest") +
    theme_minimal()
})

output$predictiveCoefPlot <- renderPlot({
  ggplot(pred_data$lm_coef, aes(x = reorder(term, abs_estimate), y = estimate, fill = estimate > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
    labs(
      title = "Top 15 Linear Regression Coefficients",
      x = "Predictor",
      y = "Coefficient Estimate"
    ) +
    theme_minimal()
})

output$predictiveTreePlot <- renderPlot({
  # Load the newly saved predictive tree model
  pred_tree <- readRDS(file.path(data_root, "predictive_tree_final.rds"))
  draw_tree_plot(
    extract_fit_engine(pred_tree),
    palette = "Blues"
  )
})


output$rawDataTable <- renderDT({
  datatable(
    customers, 
    options = list(
      pageLength = 10,       # Shows 10 rows per page
      scrollX = TRUE,        # Adds a horizontal scrollbar if the table is wide
      dom = 'Bfrtip'         # Adds basic search and filtering controls
    ),
    class = 'cell-border stripe' # Makes it look clean and readable
  )
})

}


shinyApp(ui, server)