# Title     : TODO
# Objective : TODO
# Created by: args06
# Created on: 27/01/2021

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

sentimentAMD <- read.csv("result_amd.csv")
sentimentNVDA <- read.csv("result_nvda.csv")
sentimentGOOGL <- read.csv("result_googl.csv")
stockName <- c("AMD", "Google", "NVIDIA")

ui <- fluidPage(
  title = "Sentimen Analisis",
  headerPanel("Sentimen Analisis Emiten Saham"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "stockName",
                  label = "Stock Name",
                  choices = stockName,
                  selected = stockName[1]),
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Sentiment Plot", plotlyOutput("plot")),
                  tabPanel("Sentiment Table", dataTableOutput('table')
                  )
      )
    )
  )
)

server <- function(input, output) {

  plotting <- reactive({
    if (input$stockName == "AMD") {
      selected <- sentimentAMD
    } else if (input$stockName == "Google") {
      selected <- sentimentGOOGL
    } else {
      selected <- sentimentNVDA
    }

    selected %>%
      ggplot(aes(x = sentiment_nya, fill = sentiment_nya)) +
      geom_bar() +
      labs(title = input$stockName) +
      theme_light()
  })

  output$plot <- renderPlotly({
    ggplotly(plotting())
  })

  tabling <- function(stockName) {
    if (stockName == "AMD") {
      selected <- sentimentAMD
    } else if (stockName == "Google") {
      selected <- sentimentGOOGL
    } else {
      selected <- sentimentNVDA
    }
    df <- select(selected, tweet, sentiment_nya)
    df
  }

  output$table <- renderDataTable({
    datatable(tabling(input$stockName))
  })
}

shinyApp(ui = ui, server = server)