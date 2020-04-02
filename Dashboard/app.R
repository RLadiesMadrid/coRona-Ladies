library(shiny)
library(tidyverse)
library(shinydashboard)

ui <- dashboardPage(
   dashboardHeader(title = "coRona-Ladies"),

   dashboardSidebar(
   checkboxGroupInput(inputId = "CCAA",  "Comunidades Autónomas:",
                      levels(CCAA_levels), "España")
   ),
   

   dashboardBody(
   plotOutput(outputId = "plot")
   )
   )

server <- function(input, output) {
   
   output$list <- renderText({
      input$CCAA
   })
   
   output$plot <- renderPlot({
     req(input$CCAA)       # Evita valores nulos
     covid_casos_g <- covid_casos %>% filter(CCAA == input$CCAA)
     ggplot(covid_casos_g, aes(x = fecha, y = total, color = CCAA, label = CCAA)) +
               geom_point() +
               geom_line(aes(group = CCAA)) +
               theme(axis.text.x = element_text(angle = 45))
   })
   
}

shinyApp(ui = ui, server = server)


