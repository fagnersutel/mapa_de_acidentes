library(ggplot2)
library(ggthemes)
library(viridis) # devtools::install_github("sjmgarnier/viridis)
library(ggmap)
library(scales)
library(grid)
library(stringr)
library(dplyr)
library(gridExtra)
library(leaflet.extras)
setwd("/Users/fsmoura/Desktop/docs/")
filenames <- list.files(path = "/Users/fsmoura/Desktop/docs/geo/") 
data <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
dim(data)
data <- data[data$tipo == "GARAGEM E ESTACIONAMENTO PARA VEICULOS, EXCETO OS DE CARGA OU COLE", ]
dim(data)

data = data %>%
  filter(str_detect(formatted_address, "Centro"))
dim(data)
data <- data[data$long < -51.21880, ]
dim(data) #
data <- data[data$lat > -30.0391003, ]
dim(data)
names(data)

leaflet(data) %>%
  addTiles(group="OSM") %>%
  addCircles(~long, ~lat, popup=~paste("ID: ", default, "Local: ", formatted_address, sep = " "),  
             weight = 1, radius=20, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomright", colors= "#ffa500", labels="2013-2016", title="Acidentes:")


files <- paste("garagens_", as.character(as.numeric(Sys.time())), ".csv", sep = "")
write.table(data,file=files,sep=";",dec = " ", row.names=FALSE)



  dados <- cbind(data$long, data$lat, data$default, data$tipo)
dados <- as.data.frame(dados)
dados$V1 <- as.numeric(as.character(dados$V1))
dados$V2 <- as.numeric(as.character(dados$V2))
dados <- dados[dados$V2 < 0, ]
dados <- subset(dados, !is.na(V1))
dim(dados)
dados$newrow <- sample(1000, size = nrow(dados), replace = TRUE)
dados$cut <- cut(dados$newrow, breaks=seq(0,1000,200), labels=sprintf("Score %d-%d",seq(0, 800, 200), seq(200,1000,200)))


leaflet(dados) %>%
    addTiles(group="OSM") %>%
    addHeatmap(group="heat", lng=dados$V1, lat=dados$V2, max=.5, blur = 60) %>% 
    addCircles(~V1, ~V2, popup=dados$newrow,  weight = 0.1, radius=1, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
    addLegend("bottomright", colors= "#ffa500", labels="Centro Histórico", title="Estacionamentos Pagos")
    #%>%
    #addMarkers(~V1, ~V2, popup = ~as.character(newrow), label = ~as.character(newrow))


