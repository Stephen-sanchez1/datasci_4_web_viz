# Import necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Depression Age-adjusted Prevalence in NJ by County"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county", "Choose a county:", choices = NULL)
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Load the dataset
  df <- reactive({
    url <- "https://raw.githubusercontent.com/Stephen-sanchez1/datasci_4_web_viz/main/datasets/PLACES__Local_Data_for_Better_Health__County_Data_2023_release%20(1).csv"
    read.csv(url)
  })
  
  # Filter the dataset
  df_depression <- reactive({
    data <- df()
    filter(data, MeasureId == "DEPRESSION", Data_Value_Type == "Age-adjusted prevalence")
  })
  
  # Update county choices dynamically based on dataset
  observe({
    depression_data <- df_depression()
    updateSelectInput(session, "county", choices = sort(unique(depression_data$LocationName)))
  })
  
  # Render the bar plot
  output$barPlot <- renderPlot({
    depression_data <- df_depression()
    county_data <- depression_data[depression_data$LocationName == input$county, ]
    avg_value <- mean(depression_data$Data_Value, na.rm = TRUE)
    
    ggplot() +
      geom_bar(data = county_data, aes(x = LocationName, y = Data_Value, fill = LocationName), stat = "identity") +
      geom_hline(aes(yintercept = avg_value), linetype = "dashed", color = "dodgerblue") +
      labs(title = 'Depression Age-adjusted Prevalence',
           y = 'Data Value (Age-adjusted prevalence) - Percent',
           x = 'Location (County)') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 60) +
      scale_fill_manual(values = c("lightcoral", "dodgerblue"))
  })
  
}
# Run the Shiny app
shinyApp(ui, server)