library(shiny)
library(tidyverse)
library(shinydashboard)


fecha_ultima <- covid_casos %>% filter(fecha == max(fecha) & CCAA == "España") %>% select(fecha)

ui <- dashboardPage(skin="purple",

   
   dashboardHeader(title = "coRona-Ladies"),

   dashboardSidebar(
   checkboxGroupInput(inputId = "CCAA",  "Comunidades Autónomas:", levels(CCAA_levels), "España"),
   img(src = "R-Ladies.png",
       height = "50%", width = "50%"),
   img(src = "UE_horz_fondo negro.png",
       height = "100%", width = "100%")
   ),

   
   
   dashboardBody(

      fluidRow(
         checkboxGroupInput(inputId = "Tipo", "Tipo de Datos:", c("Casos confirmados", "Fallecidos", "Ingresos en UCI", "Altas hospitalarias", "Hospitalizados"),
                         c("Casos confirmados"), inline = TRUE)
         ),
      
      fluidRow(
         plotOutput(outputId = "plot"),
         ),

      h3("Datos para España a fecha de", align = "center", fecha_ultima),
      
      fluidRow(
         valueBoxOutput("casos_esp"),
         valueBoxOutput("fallecidos_esp"),
         valueBoxOutput("uci_esp"),
         valueBoxOutput("altas_esp"),
         valueBoxOutput("hospitalizados_esp")
      )
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

   
   output$casos_esp <- renderValueBox({
      casos_esp_hoy <- covid_casos %>% filter(fecha == max(fecha) & CCAA == "España") %>% select(total)
      valueBox("Casos totales",
               value = casos_esp_hoy,
               icon = icon("male"),
               color = "purple")
   })
   
   output$fallecidos_esp <- renderValueBox({
      fallecidos_esp_hoy <- covid_fallecidos %>% filter(fecha == max(fecha) & CCAA == "España") %>% select(total)
      valueBox("Fallecidos",
               value = fallecidos_esp_hoy,
               icon = icon("frown"),
               color = "red")
   })
   
   output$uci_esp <- renderValueBox({
      uci_esp_hoy <- covid_uci %>% filter(fecha == max(fecha) & CCAA == "España") %>% select(total)
      valueBox("Ingresados en UCI",
               value = uci_esp_hoy,
               icon = icon("users-cog"),
               color = "purple")
   })
      
   output$altas_esp <- renderValueBox({
      altas_esp_hoy <- covid_altas %>% filter(fecha == max(fecha) & CCAA == "España") %>% select(total)
      valueBox("Altas hospitalarias",
               value = altas_esp_hoy,
               icon = icon("smile"),
               color = "purple")
   })

   output$hospitalizados_esp <- renderValueBox({
      hospitalizados_esp_hoy <- covid_hospitalizados %>% filter(fecha == max(fecha) & CCAA == "España") %>% select(total)
      valueBox("Hospitalizados",
               value = hospitalizados_esp_hoy,
               icon = icon("bed"),
               color = "purple")
   })
}
   

shinyApp(ui = ui, server = server)




#################################


