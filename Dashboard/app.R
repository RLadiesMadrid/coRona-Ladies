library(shiny)
library(tidyverse)
library(shinydashboard)

################  CARGAR Y PROCESAR LA BASE DE DATOS  ####################
# Leer datos
# Link tablas (sacadas de código fuente)
covid_casos <- read.csv2("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_casos_long.csv",
                         sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8", header = TRUE)
covid_fallecidos <- read.csv2("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_fallecidos_long.csv",
                              sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8", header = TRUE)
covid_uci <- read.csv2("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_uci_long.csv",
                       sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8", header = TRUE)
covid_altas <- read.csv2("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_altas_long.csv",
                         sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8", header = TRUE)
covid_hospitalizados <- read.csv2("https://github.com/datadista/datasets/raw/master/COVID%2019/ccaa_covid19_hospitalizados_long.csv",
                                  sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8", header = TRUE)



# Fecha como Date (en lugar de char)
covid_casos$fecha <- as.Date(covid_casos$fecha, "%Y-%m-%d")
covid_fallecidos$fecha <- as.Date(covid_fallecidos$fecha, "%Y-%m-%d")
covid_uci$fecha <- as.Date(covid_uci$fecha, "%Y-%m-%d")
covid_altas$fecha <- as.Date(covid_altas$fecha, "%Y-%m-%d")
covid_hospitalizados$fecha <- as.Date(covid_hospitalizados$fecha, "%Y-%m-%d")

# Factorizar CCAA para Dashboard
covid_casos$CCAA <- as.factor(covid_casos$CCAA)
covid_fallecidos$CCAA <- as.factor(covid_fallecidos$CCAA)
covid_uci$CCAA <- as.factor(covid_uci$CCAA)
covid_altas$CCAA <- as.factor(covid_altas$CCAA)
covid_hospitalizados$CCAA <- as.factor(covid_hospitalizados$CCAA)

# Cambiar level "Total" por "España"
levels(covid_casos$CCAA)[levels(covid_casos$CCAA)=="Total"] <- "España"
levels(covid_fallecidos$CCAA)[levels(covid_fallecidos$CCAA)=="Total"] <- "España"
levels(covid_uci$CCAA)[levels(covid_uci$CCAA)=="Total"] <- "España"
levels(covid_altas$CCAA)[levels(covid_altas$CCAA)=="Total"] <- "España"
levels(covid_hospitalizados$CCAA)[levels(covid_hospitalizados$CCAA)=="Total"] <- "España"

# Reordenamos levels
CCAA_levels <- levels(covid_casos$CCAA)
CCAA_levels <- levels(covid_fallecidos$CCAA)
CCAA_levels <- levels(covid_uci$CCAA)
CCAA_levels <- levels(covid_altas$CCAA)
CCAA_levels <- levels(covid_hospitalizados$CCAA)
CCAA_levels <- ordered(CCAA_levels, levels = c("España", "Andalucía", "Aragón",
                                               "Asturias", "Baleares", "C. Valenciana",
                                               "Canarias", "Cantabria", "Castilla y León",
                                               "Castilla-La Mancha", "Cataluña", "Ceuta",
                                               "Extremadura", "Galicia", "La Rioja", "Madrid",
                                               "Melilla", "Murcia", "Navarra", "País Vasco"))   
# Ponemos "España" en primer lugar para el Dashboard
covid_casos <- rbind(covid_casos[covid_casos$CCAA=="España",],covid_casos[covid_casos$CCAA!="España",])
covid_fallecidos <- rbind(covid_fallecidos[covid_fallecidos$CCAA=="España",],covid_fallecidos[covid_fallecidos$CCAA!="España",])
covid_uci <- rbind(covid_uci[covid_uci$CCAA=="España",],covid_uci[covid_uci$CCAA!="España",])
covid_altas <- rbind(covid_altas[covid_altas$CCAA=="España",],covid_altas[covid_altas$CCAA!="España",])
covid_hospitalizados <- rbind(covid_hospitalizados[covid_hospitalizados$CCAA=="España",],covid_hospitalizados[covid_hospitalizados$CCAA!="España",])



###   Fusión de bases (para simplificar código de Dashboard)  ###

# Añadir variable de tipo de datos
covid_casos$tipo <- rep("casos", nrow(covid_casos))
covid_fallecidos$tipo <- rep("fallecidos", nrow(covid_fallecidos))
covid_uci$tipo <- rep("uci", nrow(covid_uci))
covid_altas$tipo <- rep("altas", nrow(covid_altas))
covid_hospitalizados$tipo <- rep("hospitalizados", nrow(covid_hospitalizados))

# fusión
covid_total <- rbind(covid_casos, covid_fallecidos, covid_uci, covid_altas, covid_hospitalizados)

##########################################################################





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


