library(shiny)
library(plotly)

ui <- fluidPage(
   titlePanel("Dashboard coRona-Ladies"),
   checkboxGroupInput("CCAA", "Comunidades Autónomas:",
                      levels(CCAA_levels), "España"),
   plotOutput(outputId = "plot")
        )
   
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplotly(ggplot(covid_casos, aes(x = fecha, y = total, color = CCAA, label = CCAA)) +
               geom_point() +
               geom_line(aes(group = CCAA)) +
               theme(axis.text.x = element_text(angle = 45)))
  })
  
   }

shinyApp(ui = ui, server = server)


