library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

data("iris")

ui <- fluidPage(
  titlePanel("Assignment 1 - Reactive Shiny App (Iris)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "species",
        label = "Species:",
        choices = c("All", levels(iris$Species)),
        selected = "All"
      ),
      
      selectInput(
        inputId = "xvar",
        label = "X variable:",
        choices = names(iris)[1:4],
        selected = "Sepal.Length"
      ),
      
      selectInput(
        inputId = "yvar",
        label = "Y variable:",
        choices = names(iris)[1:4],
        selected = "Petal.Length"
      ),
      
      sliderInput(
        inputId = "bins",
        label = "Histogram bins:",
        min = 5, max = 50, value = 20
      ),
      
      checkboxInput(
        inputId = "showTable",
        label = "Show data table",
        value = TRUE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter plot", plotOutput("scatter")),
        tabPanel("Histogram", plotOutput("hist")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Data",
                 conditionalPanel(
                   condition = "input.showTable == true",
                   DTOutput("table")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dataset reactivo filtrado (cambia según species)
  filtered_data <- reactive({
    if (input$species == "All") {
      iris
    } else {
      iris %>% filter(Species == input$species)
    }
  })
  
  # Scatter plot reactivo (cambia según xvar/yvar/species)
  output$scatter <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]], color = Species)) +
      geom_point(size = 2) +
      labs(x = input$xvar, y = input$yvar) +
      theme_minimal()
  })
  
  # Histograma reactivo (cambia según xvar/bins/species)
  output$hist <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = .data[[input$xvar]])) +
      geom_histogram(bins = input$bins) +
      labs(x = input$xvar, y = "Count") +
      theme_minimal()
  })
  
  # Resumen reactivo (depende de filtros y variables)
  output$summary <- renderPrint({
    df <- filtered_data()
    
    df %>%
      summarise(
        n = n(),
        mean_x = mean(.data[[input$xvar]]),
        sd_x   = sd(.data[[input$xvar]]),
        mean_y = mean(.data[[input$yvar]]),
        sd_y   = sd(.data[[input$yvar]])
      )
  })
  
  # Tabla reactiva (depende de filtros)
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
