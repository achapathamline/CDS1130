library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rsconnect)

rsconnect::setAccountInfo(name = "13a4m1-xander-chapman", token = "3E5754F64A48CBF2BA26BA2C7146DD80", secret = "f6zXeGda8dpE1NVjziL3DEayEz0NpfXPV9/pASir")

set.seed(42)
data <- data.frame(
  Year = 2000:2020,
  Value = runif(21, min = 0, max = 100)
)

ui <- fluidPage(
  titlePanel("Bar Chart with Slider"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", "Select Year Range:",
                  min = min(data$Year),
                  max = max(data$Year),
                  value = c(min(data$Year), max(data$Year)),
                  step = 1)
    ),
    mainPanel(
      plotOutput("barChart")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(Year >= input$yearRange[1] & Year <= input$yearRange[2])
  })

  output$barChart <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Value, fill = color)) +
      geom_bar(stat = "identity") +
      labs(title = "Bar Chart of Values",
           x = "Year",
           y = "Value")
  })
}


rsconnect::deployApp()