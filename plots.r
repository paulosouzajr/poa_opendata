require("dplyr")
require("ggplot2")
require(scales)
library(plotly)
require(lubridate)
require(tidyverse)

#setwd("~/Documents/poa_opendata") #Necessario adicionar path onde os arquivos estao
setwd("~/poa_opendata/dataset")

fNames <- list.files(pattern = "acidentes-2*")

acidentes <- lapply(fNames, function(fNames){
  data.frame(read.csv(fNames, header=TRUE, sep=";"))
})

aux <- data.frame()
year <- 2000

for(i in 1:14){
  acidentes[[i]]$year <- year
  aux <- rbind(aux, select(acidentes[[i]], DATA_HORA, TEMPO, UPS, year))
  year = year + 1
}

aux$freq <- 1

aux <- aux %>% group_by(year, UPS, TEMPO, time = floor_date(as.POSIXct(format(strptime(DATA_HORA, "%Y%m%d %H:%M"), format='%H:%M'), format="%H:%M"), "1 hours")) %>%
  summarise(freq = n())

ggplot(aux, aes(x = as.POSIXct(time, format="%H:%M"), 
                y = freq, colour= TEMPO)) + geom_jitter()+ facet_wrap(~year) + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  ggtitle("Total de acidentes x Hora x Clima") 

ggplot(aux, aes(x = as.POSIXct(time, format="%H:%M"), 
                y = freq, colour= as.factor(UPS))) + geom_jitter()+ facet_wrap(~year) + geom_smooth(method = "auto") +
  ggtitle("Total de acidentes x Hora x Severidade") 

ggplot(aux, aes(x = as.POSIXct(time, format="%H:%M"), 
                y = as.factor(UPS), colour= TEMPO)) + geom_h()+ facet_wrap(~year) +
  ggtitle("Total de acidentes x Hora x Severidade") +  theme_minimal()

ggplot(aux, aes(x = as.POSIXct(time, format="%H:%M"), 
                y = freq, fill= TEMPO)) + geom_bar(stat = 'identity', position = 'dodge')  +
  labs(x="Hora") + facet_wrap(~year)


res <- aux %>% group_by(time, UPS, TEMPO) %>%
  summarise(freq = mean(freq, na.rm = TRUE), se=3*sd(time)/sqrt(n()))

p <- plot_ly(aux, x = ~time, y = ~as.factor(UPS), z = ~freq, color = ~TEMPO, type = 'scatter3d', mode = 'lines') %>%
  layout(scene = list(xaxis = list(title = 'time'),
                      yaxis = list(title = 'UPS'),
                      zaxis = list(title = 'count')))



