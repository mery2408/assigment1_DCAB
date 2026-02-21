# app.R
# Reactive Shiny App (Iris) - Dynamic controls per tab + Summary/Data extra controls

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
      
      # Always-visible controls
      selectInput(
        inputId = "species",
        label   = "Filter species",
        choices = c("All", levels(iris$Species)),
        selected = "All"
      ),
      
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
      
      # Tab-specific controls
      uiOutput("sidebar_dynamic")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tab",
        
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
  
  # Nice consistent palette
  species_cols <- c(
    setosa = "#E45756",
    versicolor = "#4C78A8",
    virginica = "#54A24B"
  )
  
  # Dynamic controls by active tab
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
    } else if (input$tab == "Summary") {
      tagList(
        selectInput(
          inputId = "summary_var",
          label   = "Variable to summarise",
          choices = names(iris)[1:4],
          selected = "Sepal.Length"
        ),
        checkboxInput(
          inputId = "group_species",
          label   = "Group summary by species",
          value   = TRUE
        )
      )
    } else if (input$tab == "Data") {
      tagList(
        sliderInput(
          inputId = "n_rows",
          label   = "Number of rows to display",
          min     = 5, max = nrow(iris), value = 10, step = 1
        ),
        checkboxGroupInput(
          inputId = "columns",
          label   = "Columns to display",
          choices = names(iris),
          selected = names(iris)
        )
      )
    } else {
      NULL
    }
  })
  
  # Safe defaults when those inputs are not created yet
  get_pt_size <- reactive({ if (is.null(input$pt_size)) 2 else input$pt_size })
  get_show_reg <- reactive({ isTRUE(input$show_reg) })
  
  get_hist_var <- reactive({ if (is.null(input$hist_var)) "Sepal.Length" else input$hist_var })
  get_bins <- reactive({ if (is.null(input$bins)) 20 else input$bins })
  
  get_summary_var <- reactive({ if (is.null(input$summary_var)) "Sepal.Length" else input$summary_var })
  get_group_species <- reactive({ isTRUE(input$group_species) })
  
  get_n_rows <- reactive({ if (is.null(input$n_rows)) 10 else input$n_rows })
  get_columns <- reactive({ if (is.null(input$columns) || length(input$columns) == 0) names(iris) else input$columns })
  
  # Global filtered dataset by species
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
    
    validate(need(input$xvar != input$yvar, "Please choose different variables for X and Y."))
    
    p <- ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]], color = Species)) +
      geom_point(size = get_pt_size(), alpha = 0.85) +
      scale_color_manual(values = species_cols) +
      labs(x = input$xvar, y = input$yvar, title = "Scatter plot (reactive)") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "right")
    
    if (get_show_reg()) p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8)
    
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
      theme(plot.title = element_text(face = "bold"), legend.position = "right")
  })
  
  # Summary (dynamic: selected variable, optionally grouped by species)
  output$summary_txt <- renderPrint({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    sv <- get_summary_var()
    
    cat("Filtered dataset info\n")
    cat("---------------------\n")
    cat("Rows:", nrow(df), "\n")
    cat("Species:", paste(unique(df$Species), collapse = ", "), "\n\n")
    
    cat("Selected variable:", sv, "\n")
    cat("----------------------------\n\n")
    
    # Overall stats for selected variable
    stats <- df %>%
      summarise(
        n = n(),
        mean = mean(.data[[sv]]),
        median = median(.data[[sv]]),
        sd = sd(.data[[sv]]),
        min = min(.data[[sv]]),
        max = max(.data[[sv]])
      )
    
    cat("Overall stats\n")
    cat("------------\n")
    print(stats)
    
    # Optional group by species
    if (get_group_species()) {
      cat("\nGrouped by species\n")
      cat("------------------\n")
      grp <- df %>%
        group_by(Species) %>%
        summarise(
          n = n(),
          mean = mean(.data[[sv]]),
          median = median(.data[[sv]]),
          sd = sd(.data[[sv]]),
          min = min(.data[[sv]]),
          max = max(.data[[sv]]),
          .groups = "drop"
        )
      print(grp)
    }
  })
  
  # Data table (dynamic: columns + number of rows)
  output$table_ui <- renderUI({
    if (has_DT) DT::DTOutput("data_tbl") else tableOutput("data_tbl_base")
  })
  
  if (has_DT) {
    output$data_tbl <- DT::renderDT({
      df <- filtered_data()
      req(nrow(df) > 0)
      
      cols <- get_columns()
      validate(need(all(cols %in% names(df)), "Selected columns not found."))
      
      df2 <- df[, cols, drop = FALSE]
      df2 <- head(df2, get_n_rows())
      
      DT::datatable(df2, options = list(pageLength = min(10, nrow(df2))), rownames = FALSE)
    })
  } else {
    output$data_tbl_base <- renderTable({
      df <- filtered_data()
      req(nrow(df) > 0)
      
      cols <- get_columns()
      df2 <- df[, cols, drop = FALSE]
      head(df2, get_n_rows())
    })
  }
}

shinyApp(ui, server)