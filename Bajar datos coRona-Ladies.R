################################
#                              #
#     COVID-19 con R           #
#                              #
################################


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


