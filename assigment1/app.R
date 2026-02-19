# app.R
# Reactive Shiny App (Iris) - Dynamic sidebar controls per tab

library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)

has_DT <- requireNamespace("DT", quietly = TRUE)
data("iris")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C7FB8",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    font_scale = 1.1
  ),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f7f9fc; }
      .well {
        background: white;
        border-radius: 14px;
        box-shadow: 0 6px 20px rgba(0,0,0,0.06);
        border: 0;
      }
      .tabbable > .nav > li > a { border-radius: 10px 10px 0 0; }
      .tab-content {
        background: white;
        padding: 16px;
        border-radius: 0 14px 14px 14px;
        box-shadow: 0 6px 20px rgba(0,0,0,0.06);
      }
      h2 { font-weight: 700; }
      .help-block, .helpText { color: #6b7280; }
      .control-label { font-weight: 600; }
    "))
  ),
  
  titlePanel("Reactive Shiny App - Iris Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      
      # Controls that always appear
      selectInput(
        inputId = "species",
        label   = "Filter species",
        choices = c("All", levels(iris$Species)),
        selected = "All"
      ),
      
      # Always: X/Y for scatter (and also used by summary if you want)
      selectInput(
        inputId = "xvar",
        label   = "X variable",
        choices = names(iris)[1:4],
        selected = "Sepal.Length"
      ),
      selectInput(
        inputId = "yvar",
        label   = "Y variable",
        choices = names(iris)[1:4],
        selected = "Petal.Length"
      ),
      
      hr(),
      
      # Controls that change depending on the active tab
      uiOutput("sidebar_dynamic"),
      
      hr(),
      
      # Always: show/hide table (affects Data tab)
      checkboxInput(
        inputId = "show_table",
        label   = "Show data table",
        value   = TRUE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tab",  # IMPORTANT: lets us know which tab is active
        tabPanel(
          "Scatter plot",
          br(),
          plotOutput("scatter", height = "450px"),
          helpText("Tip: Change X/Y variables and species to see reactive updates.")
        ),
        tabPanel(
          "Histogram",
          br(),
          plotOutput("hist", height = "450px"),
          helpText("Tip: Change histogram variable and bins to update the histogram.")
        ),
        tabPanel(
          "Summary",
          br(),
          verbatimTextOutput("summary_txt")
        ),
        tabPanel(
          "Data",
          br(),
          uiOutput("table_ui")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  species_cols <- c(
    setosa = "#E45756",
    versicolor = "#4C78A8",
    virginica = "#54A24B"
  )
  
  # Sidebar controls that depend on the active tab
  output$sidebar_dynamic <- renderUI({
    req(input$tab)
    
    if (input$tab == "Scatter plot") {
      tagList(
        sliderInput(
          inputId = "pt_size",
          label   = "Point size",
          min     = 1, max = 6, value = 2, step = 0.5
        ),
        checkboxInput(
          inputId = "show_reg",
          label   = "Show regression line (lm)",
          value   = FALSE
        )
      )
    } else if (input$tab == "Histogram") {
      tagList(
        selectInput(
          inputId = "hist_var",
          label   = "Histogram variable",
          choices = names(iris)[1:4],
          selected = "Sepal.Length"
        ),
        sliderInput(
          inputId = "bins",
          label   = "Histogram bins",
          min     = 5, max = 60, value = 20, step = 1
        )
      )
    } else {
      # Summary or Data tab: no extra controls
      helpText("No additional controls for this tab.")
    }
  })
  
  # Provide defaults in case user opens the app and server runs before controls exist
  get_pt_size <- reactive({ if (is.null(input$pt_size)) 2 else input$pt_size })
  get_show_reg <- reactive({ isTRUE(input$show_reg) })
  
  get_hist_var <- reactive({ if (is.null(input$hist_var)) "Sepal.Length" else input$hist_var })
  get_bins <- reactive({ if (is.null(input$bins)) 20 else input$bins })
  
  # Filtered dataset (global)
  filtered_data <- reactive({
    req(input$species)
    
    if (input$species == "All") iris
    else iris %>% filter(Species == input$species)
  })
  
  # Scatter plot
  output$scatter <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    req(input$xvar, input$yvar)
    
    validate(
      need(input$xvar != input$yvar, "Please choose different variables for X and Y.")
    )
    
    p <- ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]], color = Species)) +
      geom_point(size = get_pt_size(), alpha = 0.85) +
      scale_color_manual(values = species_cols) +
      labs(x = input$xvar, y = input$yvar, title = "Scatter plot (reactive)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
    
    if (get_show_reg()) {
      p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8)
    }
    
    p
  })
  
  # Histogram
  output$hist <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    hv <- get_hist_var()
    b <- get_bins()
    
    ggplot(df, aes(x = .data[[hv]], fill = Species)) +
      geom_histogram(bins = b, alpha = 0.55, position = "identity") +
      scale_fill_manual(values = species_cols) +
      labs(x = hv, y = "Count", title = "Histogram (reactive)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # Summary
  output$summary_txt <- renderPrint({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    cat("Filtered dataset info\n")
    cat("---------------------\n")
    cat("Rows:", nrow(df), "\n")
    cat("Species:", paste(unique(df$Species), collapse = ", "), "\n\n")
    
    cat("Summary of numeric variables\n")
    cat("----------------------------\n")
    print(summary(df[, 1:4]))
    
    cat("\nMean values by species\n")
    cat("---------------------\n")
    print(df %>%
            group_by(Species) %>%
            summarise(across(where(is.numeric), mean), .groups = "drop"))
  })
  
  # Data table
  output$table_ui <- renderUI({
    if (!isTRUE(input$show_table)) {
      return(helpText("Table is hidden. Enable 'Show data table' from the sidebar."))
    }
    if (has_DT) {
      DT::DTOutput("data_tbl")
    } else {
      tableOutput("data_tbl_base")
    }
  })
  
  if (has_DT) {
    output$data_tbl <- DT::renderDT({
      df <- filtered_data()
      req(nrow(df) > 0)
      DT::datatable(df, options = list(pageLength = 10), rownames = FALSE)
    })
  } else {
    output$data_tbl_base <- renderTable({
      df <- filtered_data()
      req(nrow(df) > 0)
      head(df, 50)
    })
  }
}

shinyApp(ui, server)
