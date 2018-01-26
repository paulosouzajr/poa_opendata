require("dplyr")
require("ggplot2")
require(scales)
library(plotly)
require(lubridate)
require(tidyverse)
require(reshape2)

#setwd("~/Documents/pmweb/dataset") #Necessario adicionar path onde os arquivos estao
setwd("~/poa_opendata/dataset")

fNames <- list.files(pattern = "acidentes-2*")

acidentes <- lapply(fNames, function(fNames){
  data.frame(read.csv(fNames, header=TRUE, sep=";"))
})

aux <- data.frame()
year <- 2000

for(i in 1:17){
  acidentes[[i]]$year <- year
  if(i > 15){
    acidentes[[i]]$DATA_HORA <- format(as.POSIXlt(acidentes[[i]]$DATA_HORA, format="%Y-%m-%dT%H:%M"), "%Y%m%d %H:%M")
  }
  aux <- rbind(aux, select(acidentes[[i]], DATA_HORA, TEMPO, UPS, year))
  year = year + 1
}


aux$freq <- 1

aux <- aux %>% group_by(year, UPS, TEMPO, time = floor_date(as.POSIXct(format(strptime(DATA_HORA, "%Y%m%d %H:%M"), format='%H:%M'), format="%H:%M"), "1 hours")) %>%
  summarise(freq = n())

ggplot(aux, aes(x = time, 
                y = UPS, shape= TEMPO, colour=TEMPO)) + geom_jitter()+ facet_wrap(~year, ncol = 3)+ ggtitle("Total de acidentes x Hora x Clima") 

ggplot(aux, aes(x = as.POSIXct(time, format="%H:%M"), 
                y = freq, colour= as.factor(UPS))) + geom_jitter()+ facet_wrap(~year) + geom_smooth(method = "auto") +
  ggtitle("Total de acidentes x Hora x Severidade") 

ggplot(aux, aes(x = as.POSIXct(time, format="%H:%M"), 
                y = as.factor(UPS), colour= TEMPO)) + geom_h()+ facet_wrap(~year) +
  ggtitle("Total de acidentes x Hora x Severidade") +  theme_minimal()

ggplot(aux, aes(x = as.POSIXct(time, format="%H:%M"), 
                y = freq, fill= TEMPO)) + geom_bar(stat = 'identity', position = 'dodge')  +
  labs(x="Hora") + facet_wrap(~UPS, nrow=3, scales="free_y") +
  scale_x_datetime(date_breaks = "1 hour",
                   date_labels = "%H:%M") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(aux, aes(x = as.factor(UPS), 
                y = freq, fill= TEMPO)) + geom_boxplot()  +
  labs(x="Hora")

aux$UPS <- as.factor(aux$UPS)

res <- aux %>% group_by(time, UPS, TEMPO) %>%
  summarize(freq = mean(freq), se=3*sd(time)/sqrt(n()))

p <- plot_ly(aux, x = ~time, y = ~UPS, z = ~freq, color = ~TEMPO, type = 'scatter3d', mode = 'lines') %>%
  layout(scene = list(xaxis = list(title = 'time'),
                      yaxis = list(title = 'UPS'),
                      zaxis = list(title = 'count')))

ggplot(aux, aes(x = time, 
                y = UPS, shape= TEMPO, colour=TEMPO)) + geom_jitter()+ facet_wrap(~year, ncol = 3)+ ggtitle("Total de acidentes x Hora x Clima") 

perdas <- data.frame()

year <- 2000
for(i in 1:17){
  acidentes[[i]]$year <- year
  if(i < 15){
    acidentes[[i]]$ONIBUS_MET <- 0
    acidentes[[i]]$FERIDOS_GR <- 0
  }
  perdas <- rbind(perdas, select(acidentes[[i]], AUTO, TAXI, LOTACAO, 
                                 ONIBUS_URB, ONIBUS_MET, ONIBUS_INT, CAMINHAO, 
                                 MOTO, CARROCA, BICICLETA, OUTRO, FERIDOS, FATAIS))
  year = year + 1
}



perdas <- as.data.frame(lapply(perdas, function(x) as.numeric(as.character(x))))
materiais <- perdas %>% summarize_all(funs(sum(., na.rm=TRUE))) %>% melt(id.vars=c("FERIDOS","FATAIS"))

ggplot(materiais[2:12,], aes(x = variable, 
                             y = value)) + geom_bar(stat = 'identity', position = 'dodge')  +
  labs(x="Veículo", y = "Ocorrências") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")

feridos <- data.frame(sum(perdas[perdas$AUTO >= 1, ]$FERIDOS, na.rm=TRUE), 
                      sum(perdas[perdas$TAXI >= 1, ]$FERIDOS, na.rm=TRUE),
                      sum(perdas[perdas$LOTACAO >= 1, ]$FERIDOS, na.rm=TRUE),
                      sum(perdas[perdas$ONIBUS_URB >= 1, ]$FERIDOS, na.rm=TRUE),
                      sum(perdas[perdas$ONIBUS_MET >= 1, ]$FERIDOS, na.rm=TRUE),
                      sum(perdas[perdas$ONIBUS_INT >= 1, ]$FERIDOS, na.rm=TRUE),
                      sum(perdas[perdas$CAMINHAO >= 1, ]$FERIDOS, na.rm=TRUE),
                      sum(perdas[perdas$MOTO >= 1, ]$FERIDOS, na.rm=TRUE),
                      sum(perdas[perdas$CARROCA >= 1, ]$FERIDOS, na.rm=TRUE),
                      sum(perdas[perdas$BICICLETA >= 1, ]$FERIDOS, na.rm=TRUE))

colnames(feridos) <- c("AUTO", "TAXI", "LOTACAO", "ONIBUS_URB", "ONIBUS_MET", "ONIBUS_INT", "CAMINHAO",
                       "MOTO", "CARROCA", "BICICLETA")

ggplot(melt(feridos), aes(x = variable, 
                             y = value)) + geom_bar(stat = 'identity', position = 'dodge')  +
  labs(x="Veículo", y = "Ocorrências") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")


FATAIS <- data.frame(sum(perdas[perdas$AUTO >= 1, ]$FATAIS, na.rm=TRUE), 
                      sum(perdas[perdas$TAXI >= 1, ]$FATAIS, na.rm=TRUE),
                      sum(perdas[perdas$LOTACAO >= 1, ]$FATAIS, na.rm=TRUE),
                      sum(perdas[perdas$ONIBUS_URB >= 1, ]$FATAIS, na.rm=TRUE),
                      sum(perdas[perdas$ONIBUS_MET >= 1, ]$FATAIS, na.rm=TRUE),
                      sum(perdas[perdas$ONIBUS_INT >= 1, ]$FATAIS, na.rm=TRUE),
                      sum(perdas[perdas$CAMINHAO >= 1, ]$FATAIS, na.rm=TRUE),
                      sum(perdas[perdas$MOTO >= 1, ]$FATAIS, na.rm=TRUE),
                      sum(perdas[perdas$CARROCA >= 1, ]$FATAIS, na.rm=TRUE),
                      sum(perdas[perdas$BICICLETA >= 1, ]$FATAIS, na.rm=TRUE))

colnames(FATAIS) <- c("AUTO", "TAXI", "LOTACAO", "ONIBUS_URB", "ONIBUS_MET", "ONIBUS_INT", "CAMINHAO",
                       "MOTO", "CARROCA", "BICICLETA")

ggplot(melt(FATAIS), aes(x = variable, 
                          y = value)) + geom_bar(stat = 'identity', position = 'dodge')  +
  labs(x="Veículo", y = "Ocorrências") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
