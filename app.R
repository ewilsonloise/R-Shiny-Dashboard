library(tidyverse)
library(shiny)
library(shinythemes)

bcc <- read.csv("bcc_occupation_industry - bcc_occupation_industry.csv")
ui <- fluidPage(
  titlePanel("I am adding a title!"),
  sidebarLayout(
    sidebarPanel("Here are my widgets",
                 selectInput(inputId = "suburb_select", 
                             label = "Choose a suburb:", 
                             choices = unique(bcc$suburb)
                 ),
                 radioButtons(inputId = "region_select",
                              label = "Choose region:",
                              choices = unique(bcc$region)) 
    ),
    mainPanel("put my outputs here",
              p("Suburb's top industries:"),
              tableOutput(outputId = "industry_table"),
              p("Region’s top occupations:"),
              plotOutput(outputId = "occupation_graph")
    )
  )
)

server <- function(input, output) {
  
  suburb_industry <- reactive({
    bcc %>%
      filter(suburb == input$suburb_select) %>%
      select(industry, industry_count_2041)
  })
  
  output$industry_table <- renderTable({
    suburb_industry()
  })
  
  region_occupation <- reactive({
    bcc %>%
      filter(region == input$region_select) %>%
      count(occupation, occupation_rank_2041)
  })
  
  output$occupation_graph <- renderPlot({
    ggplot(region_occupation(), aes(x = occupation, y = n, fill = occupation_rank_2041)) +
      geom_col() +
      coord_flip()
  })
  
}
shinyApp(ui = ui, server = server)
