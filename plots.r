require("dplyr")
require("ggplot2")
require(scales)
library(plotly)
require(lubridate)
require(tidyverse)
setwd("~/Documents/pmweb") #Necessário adicionar path onde os arquivos estão
fNames <- list.files(pattern = "acidentes-2*")


acidentes <- lapply(fNames, function(fNames){
  data.frame(read.csv(fNames, header=TRUE, sep=";"))
})

test <- data.frame(acidentes[[1]])

test <- select(test, DATA_HORA, TEMPO, UPS)

test$ano <- 2000

test2 <- data.frame(acidentes[[2]])

test2 <- select(test2, DATA_HORA, TEMPO, UPS)

test2$ano <- 2001

test3 <- rbind(test, test2)
test3$freq <- 1

test3 <- test3 %>% group_by(ano, UPS, TEMPO, time = floor_date(as.POSIXct(format(strptime(DATA_HORA, "%Y%m%d %H:%M"), format='%H:%M'), format="%H:%M"), "1 hours")) %>%
  summarise(freq = n())

ggplot(test3, aes(x = as.POSIXct(format(strptime(DATA_HORA, "%Y%m%d %H:%M"), format='%H:%M'), format="%H:%M"), 
                  y = as.factor(UPS), fill=TEMPO)) + 
  geom_bar(stat = 'identity', position = 'stack')  + labs(x="Hora") + scale_x_datetime(date_breaks = "2 hour",
                                                                                                                                                                                           date_labels = "%H:%M") + facet_wrap(~ano)
p <- plot_ly(test3, x = ~time, y = ~as.factor(UPS), z = ~freq, color = ~TEMPO, type = 'scatter3d', mode = 'lines') %>%
  layout(scene = list(xaxis = list(title = 'time'),
                      yaxis = list(title = 'UPS'),
                      zaxis = list(title = 'count')))



