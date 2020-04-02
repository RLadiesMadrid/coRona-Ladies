################################
#                              #
#     COVID-19 con R           #
#                              #
################################

# Librerias
library(tidyverse)

######################            LECTURA DATOS por CCAA            #####################

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

# Cambiar level "Total" por "España"
levels(covid_casos$CCAA)[levels(covid_casos$CCAA)=="Total"] <- "España"
# Reordenamos levels
CCAA_levels <- levels(covid_casos$CCAA)
CCAA_levels <- ordered(CCAA_levels, levels = c("España", "Andalucía", "Aragón",
                                               "Asturias", "Baleares", "C. Valenciana",
                                               "Canarias", "Cantabria", "Castilla y León",
                                               "Castilla-La Mancha", "Cataluña", "Ceuta",
                                               "Extremadura", "Galicia", "La Rioja", "Madrid",
                                               "Melilla", "Murcia", "Navarra", "País Vasco"))   
# Ponemos "España" en primer lugar para el Dashboard
covid_casos <- rbind(covid_casos[covid_casos$CCAA=="España",],covid_casos[covid_casos$CCAA!="España",])
