################################
#                              #
#     COVID-19 con R           #
#                              #
################################


# Activar librerías
library(tidyverse)
library(plotly)
library(ggrepel)

######################            GRÁFICO INTERACTIVO           #####################

# Casos
g1 <- ggplot(covid_casos, aes(x = fecha, y = total, color = CCAA, label = CCAA)) +
  geom_point() +
  geom_line(aes(group = CCAA)) +
  theme(axis.text.x = element_text(angle = 45))
ggplotly(g1)


# Fallecidos
g2 <- ggplot(covid_fallecidos, aes(x = fecha, y = total, color = CCAA, label = CCAA)) +
  geom_point() +
  geom_line(aes(group = CCAA)) +
  theme(axis.text.x = element_text(angle = 45))
  )
ggplotly(g2)

# Ingresos en UCI
g3 <- ggplot(covid_uci, aes(x = fecha, y = total, color = CCAA, label = CCAA)) +
  geom_point() +
  geom_line(aes(group = CCAA)) +
  theme(axis.text.x = element_text(angle = 45))
  )
ggplotly(g3)

# Número de altas
g4 <- ggplot(covid_altas, aes(x = fecha, y = total, color = CCAA, label = CCAA)) +
  geom_point() +
  geom_line(aes(group = CCAA)) +
  theme(axis.text.x = element_text(angle = 45))
ggplotly(g4)

# Pacientes hospitalizados
g5 <- ggplot(covid_hospitalizados, aes(x = fecha, y = total, color = CCAA, label = CCAA)) +
  geom_point() +
  geom_line(aes(group = CCAA)) +
  theme(axis.text.x = element_text(angle = 45))
ggplotly(g5)

#
