library(shiny)
library(tidyverse)
library(shinydashboard)

ui <- dashboardPage(

   dashboardHeader(title = "coRona-Ladies"),

   dashboardSidebar(
   checkboxGroupInput(inputId = "CCAA",  "Comunidades Autónomas:", levels(CCAA_levels), c("Cataluña", "Madrid"))
   ),
   
   
   dashboardBody(
      checkboxGroupInput(inputId = "Tipo", "Tipo de Datos:", c("Casos confirmados", "Fallecidos", "Ingresos en UCI", "Altas hospitalarias", "Hospitalizados"),
                         c("Casos confirmados", "Fallecidos", "Ingresos en UCI", "Altas hospitalarias", "Hospitalizados"), inline = TRUE),
      plotOutput(outputId = "plot")
      )
   )

server <- function(input, output) {

   output$plot <- renderPlot({
      casos          <- "Casos confirmados"        %in% input$Tipo
      fallecidos     <- "Fallecidos"               %in% input$Tipo
      uci            <- "Ingresos en UCI"          %in% input$Tipo
      altas          <- "Altas hospitalarias"      %in% input$Tipo
      hospitalizados <- "Hospitalizados"           %in% input$Tipo

      req(input$CCAA, cancelOutput = FALSE)       # Evita error por valores nulos
      
      covid_casos_g <- covid_casos %>% filter(CCAA == input$CCAA)
      covid_fallecidos_g <- covid_fallecidos %>% filter(CCAA == input$CCAA)
      covid_uci_g <- covid_uci %>% filter(CCAA == input$CCAA)
      covid_altas_g <- covid_altas %>% filter(CCAA == input$CCAA)
      covid_hospitalizados_g <- covid_hospitalizados %>% filter(CCAA == input$CCAA)
      
      if (casos & fallecidos & uci & altas & hospitalizados){
         
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 2, 3, 4, 5),
                               labels = c("Casos", "Fallecidos", "Ingresados en UCI", "Altas hospitalarias", "Hospitalizados"))
        
      } else if (casos & fallecidos & uci & altas){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 2, 3, 4),
                               labels = c("Casos", "Fallecidos", "Ingresados en UCI", "Altas hospitalarias"))
            
      } else if (casos & fallecidos & uci & altas){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 2, 3, 5),
                               labels = c("Casos", "Fallecidos", "Ingresados en UCI", "Hospitalizados"))
         
      } else if (casos & fallecidos & uci & altas){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 2, 4, 5),
                               labels = c("Casos", "Fallecidos", "Altas hospitalarias", "Hospitalizados"))
         
      } else if (casos & fallecidos & uci & altas){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 3, 4, 5),
                               labels = c("Casos", "Ingresados en UCI", "Altas hospitalarias", "Hospitalizados"))
         
      } else if (casos & fallecidos & uci & altas){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(2, 3, 4, 5),
                               labels = c("Fallecidos", "Ingresados en UCI", "Altas hospitalarias", "Hospitalizados"))
         
         
      } else if (casos & fallecidos & uci){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 2, 3),
                               labels = c("Casos", "Fallecidos", "Ingresados en UCI"))

      } else if (casos & fallecidos & uci){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 2, 4),
                               labels = c("Casos", "Fallecidos", "Altas Hospitalarias"))
         
      } else if (casos & fallecidos & uci){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 2, 5),
                               labels = c("Casos", "Fallecidos", "Hospitalizados"))
         
      } else if (casos & fallecidos & uci){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 3, 4),
                               labels = c("Casos", "Ingresados en UCI", "Altas Hospitalarias"))
         
      } else if (casos & fallecidos & uci){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 3, 5),
                               labels = c("Casos", "Ingresados en UCI", "Hospitalizados"))
         
      } else if (casos & fallecidos & uci){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 4, 5),
                               labels = c("Casos", "Altas Hospitalarias", "Hospitalizados"))
         
      } else if (casos & fallecidos & uci){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(2, 3, 4),
                               labels = c("Fallecidos", "Ingresados en UCI", "Altas Hospitalarias"))
         
      } else if (casos & fallecidos & uci){
         ggplot(NULL, aes(x = fecha, y = total)) +
           geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(2, 3, 5),
                               labels = c("Fallecidos", "Ingresados en UCI", "Hospitalizados"))
         
      } else if (casos & fallecidos & uci){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
            geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
            geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
            geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(3, 4, 5),
                               labels = c("Ingresados en UCI", "Altas Hospitalarias", "Hospitalizados"))
         
         } else if (casos & fallecidos){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
            geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = c(1, 2), labels = c("Casos", "Fallecidos"))
         
         } else if (casos & uci){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
               geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
               geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
               geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = c(1, 3), labels = c("Casos", "Ingresados en UCI"))
            
         } else if (casos & altas){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
               geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
               geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
               geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = c(1, 4), labels = c("Casos", "Altas hospitalarias"))
            
         } else if (casos & hospitalizados){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
               geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
               geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
               geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = c(1, 5), labels = c("Casos", "Hospitalizados"))
           
         } else if (fallecidos & uci){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
               geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
               geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
               geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = c(2, 3), labels = c("Fallecidos", "Ingresados en UCI"))
            
         } else if (fallecidos & altas){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
               geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
               geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
               geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = c(2, 4), labels = c("Fallecidos", "Altas hospitalarias"))
            
         } else if (fallecidos & hospitalizados){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
               geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
               geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
               geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = c(2, 5), labels = c("Fallecidos", "Hospitalizados"))
            
         } else if (uci & altas){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
               geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
               geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
               geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = c(3, 4), labels = c("Ingresados en UCI", "Altas hospitalarias"))
            
         } else if (uci & hospitalizados){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
               geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
               geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
               geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = c(3, 5), labels = c("Ingresados en UCI", "Hospitalizados"))
            
         } else if (altas & hospitalizados){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
               geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
               geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
               geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = c(4, 5), labels = c("Altas hospitalarias", "Hospitalizados"))
            
            
         } else if (casos){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_casos_g,          mapping = aes(shape = "1")) +
               geom_line(   covid_casos_g,          mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = 1, labels = "Casos")
         
         
         } else if (fallecidos){
         ggplot(NULL, aes(x = fecha, y = total)) +
            geom_point(  covid_fallecidos_g,     mapping = aes(shape = "2")) +
            geom_line(   covid_fallecidos_g,     mapping = aes(color = CCAA, group = CCAA)) +
            ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
            xlab("Fecha") +
            ylab("Número total") +
            scale_shape_manual(name = "Tipo de datos", values = 2, labels = "Fallecidos")

         
         } else if (uci){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_uci_g,            mapping = aes(shape = "3")) +
               geom_line(   covid_uci_g,            mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = 3, labels = "Ingresados en la UCI")
            
            
         } else if (altas){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_altas_g,          mapping = aes(shape = "4")) +
               geom_line(   covid_altas_g,          mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("Fallecidos de COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = 4, labels = "Altas hospitalarias")

            
         } else if (hospitalizados){
            ggplot(NULL, aes(x = fecha, y = total)) +
               geom_point(  covid_hospitalizados_g, mapping = aes(shape = "5")) +
               geom_line(   covid_hospitalizados_g, mapping = aes(color = CCAA, group = CCAA)) +
               ggtitle("COVID-19 en España por Comunidades Autónomas (2020)") +
               xlab("Fecha") +
               ylab("Número total") +
               scale_shape_manual(name = "Tipo de datos", values = 5, labels = "Hospitalizados")
            
         }
      })
}

shinyApp(ui = ui, server = server)


