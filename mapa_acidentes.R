library(ggplot2)
library(ggthemes)
library(viridis) # devtools::install_github("sjmgarnier/viridis)
library(ggmap)
library(scales)
library(grid)
library(dplyr)
library(gridExtra)
library(leaflet.extras)

setwd("/Users/fsmoura/Desktop/docs/")
data13 = read.csv("acidentes-2013.csv", header = TRUE, sep = ";")
data13$LATITUDE <- data13$LATITUDE - 0.0009
names(data13)
data13$DATA_HORA = NULL
data13 = data13[,sort(names(data13))]
dim(data13)
data14 = read.csv("acidentes-2014.csv", header = TRUE, sep = ";")
data14$LATITUDE <- data14$LATITUDE - 0.0009
names(data14)
dim(data14)
data14$DATA_HORA = NULL
data14$DATA = NULL
data14$HORA = NULL
dim(data14)
data14 = data14[,sort(names(data14))]
data15 = read.csv("acidentes-2015.csv", header = TRUE, sep = ";")
data15$LATITUDE <- data15$LATITUDE - 0.0009
names(data15)
dim(data15)
data15$DATA_HORA = NULL
data15$DATA = NULL
data15$HORA = NULL
dim(data15)
data15 = data15[,sort(names(data15))]
data16 = read.csv("acidentes-2016.csv", header = TRUE, sep = ";")
data16$LATITUDE <- data16$LATITUDE - 0.0009
names(data16)
dim(data16)
data16$DATA_HORA = NULL
data16$DATA = NULL
data16$HORA = NULL
dim(data16)
data16 = data16[,sort(names(data16))]
data17 = read.csv("acidentes-2017.csv", header = TRUE, sep = ";")
names(data17)
dim(data17)
data17$DATA = NULL
data17$HORA = NULL
dim(data17)
data17 = data17[,sort(names(data17))]

#dim(data13)
#dim(data14)
#dim(data15)
#dim(data16)
#dim(data17)
#aa = names(data13)
#bb = names(data14)
#cc = names(data15)
#dd = names(data16)
#ee = names(data17)
#aaee = cbind(aa, bb, cc, dd, ee)
#aaee

final = rbind(data13, data14, data15, data16, data17)
data = final
names(data)
dim(data)
data = data[complete.cases(data$LATITUDE), ]
data = data[complete.cases(data$LONGITUDE), ]
dim(data)
dados <- data
data <- paste("acid_poa_2013_2017", as.character(as.numeric(Sys.time())), ".csv", sep = "")
write.table(dados,file=data,sep=";",dec = " ", row.names=FALSE)

#dados = read.csv("acid_centro_poa_2013_20171533307969.95189.csv", header = TRUE, sep = ";")
dim(dados)
dados <- as.data.frame(dados)
dados <- dados[dados$REGIAO == "CENTRO", ]
dim(dados)
dados <- dados[dados$LONGITUDE < -51.21880, ]
dim(dados)
dados <- dados[dados$LATITUDE > -30.0391003, ]
dim(dados)

#data <- paste("acid_centro_poa_2013_2017", as.character(as.numeric(Sys.time())), ".csv", sep = "")
#write.table(dados,file=data,sep=";",dec = " ", row.names=FALSE)
#dados <- dados[dados$LATITUDE >  -30.039011  & dados$LONGITUDE > -51.225784, ]
dim(dados)
dados4 <- dados[dados$TIPO_ACID == "ATROPELAMENTO", ]
dim(dados4)
dados <- as.data.frame(dados)
dados$LONGITUDE <- as.numeric(as.character(dados$LONGITUDE))
#dados$LONGITUDE <- as.numeric(gsub(".", "", dados$LONGITUDE, fixed = TRUE));  dados$LONGITUDE * 1e-13
dados$LATITUDE  <- as.numeric(dados$LATITUDE)
dados <- dados[dados$LONGITUDE < -33, ]
dim(dados)
# dados1 com feridos
dim(dados)
dados1 <- dados[dados$FERIDOS > 0, ]
dim(dados1)
# dados2 feridos + feridos graves + fatais + mortes
dados2 <- dados[dados$FERIDOS > 0 | dados$FERIDOS_GR > 0 | dados$FATAIS > 0 | dados$MORTES > 0, ]
dados2$FERIDOS = as.integer(dados2$FERIDOS)
dim(dados2)
# dados3 fatais
dados3 <- dados[dados$FATAIS > 0, ]
dim(dados3)

leaflet(dados) %>%
  addTiles(group="OSM") %>%
  addCircles(~LONGITUDE, ~LATITUDE, popup=~paste("ID: ", ID,  "Local: ", LOCAL,   "<br>Ano: ", ANO, 
                                                 "<br>Tipo: ", TIPO_ACID, "<BR>N Feridos: ", FERIDOS, "<BR>N Feridos GRAVES: ", 
                                                 FERIDOS_GR, "<br>Fatais", FATAIS, "<br>Local: ", LOCAL_VIA, "<br>Lat: ", LATITUDE, "<br>Long: ", LONGITUDE,  sep = " "),  
             weight = 1, radius=20, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomright", colors= "#ffa500", labels=paste(min(dados$ANO), "-", max(dados$ANO), "<br>Acidentes:", length(dados$ANO),  "<br>Com Feridos:", length(dados$ANO), sep = " "), title="Acidentes:")


leaflet(dados1) %>%
  addTiles(group="OSM") %>%
  addCircles(~LONGITUDE, ~LATITUDE, popup=~paste("ID: ", ID,  "Local: ", LOCAL,   "<br>Ano: ", ANO, 
                                                 "<br>Tipo: ", TIPO_ACID, "<BR>N Feridos: ", FERIDOS, "<BR>N Feridos GRAVES: ", 
                                                 FERIDOS_GR, "<br>Fatais", FATAIS, "<br>Local: ", LOCAL_VIA, "<br>Lat: ", LATITUDE, "<br>Long: ", LONGITUDE,  sep = " "),  
             weight = 1, radius=20, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomright", colors= "#ffa500", labels=paste(min(dados1$ANO), "-", max(dados1$ANO), "<br>Acidentes:", length(dados$ANO),  "<br>Com Feridos:", length(dados1$ANO), sep = " "), title="Acidentes:")

leaflet(dados2) %>%
  addTiles(group="OSM") %>%
  addHeatmap(group=NULL, lng=dados2$LONGITUDE, lat=dados2$LATITUDE, radius = 30 , max=10, blur = 10) %>%
  addCircles(~LONGITUDE, ~LATITUDE, popup=~paste("ID: ", ID,  "Local: ", LOCAL,   "<br>Ano: ", ANO, 
    "<br>Tipo: ", TIPO_ACID, "<BR>N Feridos: ", FERIDOS, "<BR>N Feridos GRAVES: ", 
    FERIDOS_GR, "<br>Fatais", FATAIS, "<br>Local: ", LOCAL_VIA, "<br>Lat: ", LATITUDE, "<br>Long: ", LONGITUDE, sep = " "),  
    weight = 1, radius=10, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("topleft", colors= "#ffa500", labels=paste(min(dados2$ANO), "-", max(dados2$ANO), "<br>Total de Acidentes:", length(dados$ANO),   "<br>Com Feridos, Graves e Fatais:", length(dados2$ANO), sep = " "), title="Acidentes com Vítmas:")

leaflet(dados2) %>%
  addTiles(group="OSM") %>%
  addCircles(~LONGITUDE, ~LATITUDE, popup=~paste("ID: ", ID,  "Local: ", LOCAL,   "<br>Ano: ", ANO, 
                                                 "<br>Tipo: ", TIPO_ACID, "<BR>N Feridos: ", FERIDOS, "<BR>N Feridos GRAVES: ", 
                                                 FERIDOS_GR, "<br>Fatais", FATAIS, "<br>Local: ", LOCAL_VIA, "<br>Lat: ", LATITUDE, "<br>Long: ", LONGITUDE, sep = " "),  
             weight = 1, radius=10, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("topleft", colors= "#ffa500", labels=paste(min(dados2$ANO), "-", max(dados2$ANO), "<br>Total de Acidentes:", length(dados$ANO),   "<br>Com Feridos, Graves e Fatais:", length(dados2$ANO), sep = " "), title="Acidentes com Vítmas:")


leaflet(dados2) %>%
  addTiles(group="OSM") %>%
  addHeatmap(group=NULL, lng=dados2$LONGITUDE, lat=dados2$LATITUDE, radius = 30 , max=8, blur = 10) %>%
  addLegend("topleft", colors= "#ffa500", labels=paste(min(dados2$ANO), "-", max(dados2$ANO), "<br>Total de Acidentes:", length(dados$ANO),   "<br>Com Feridos, Graves e Fatais:", length(dados2$ANO), sep = " "), title="Acidentes com Vítmas:")

#MAPA BASE
mh_map_set_dois = get_googlemap(center = c(-51.229956979237386, -30.030948), 
                                zoom = 15, source="osm",
                                color = "color",
                                source = "google",
                                maptype = "roadmap")
#mAPA DE vITIMAS
ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE,  
                     fill = ..level..,alpha=..level..), bins = 5, 
                 geom = "polygon", data = dados2) +
  scale_alpha(guide="none") + 
  scale_fill_gradient(low = "#e3fc00", high = "#e3a500", guide="none")+
  ggtitle("Mapa de Vitimas")

  #MAPA DE ATROPELAMENTOS
ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE,  
                     fill = ..level..,alpha=..level..), bins = 5, 
                 geom = "polygon", data = dados4) +
  scale_alpha(guide="none") + 
  scale_fill_gradient(low = "#fb0010", high = "#c00010", guide="none")+
  ggtitle("Mapa de Atropelamentos")+
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


#MAPA DE ACIDENTES
ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE,  
                     fill = ..level..,alpha=..level..), bins = 6, 
                 geom = "polygon", data = dados2) +
  scale_alpha(guide="none") + 
  scale_fill_gradient(low = "#132B43", high = "blue", guide="none")+
  ggtitle("Mapa de Acidentes")

#MAPA DE VÍTIMAS
ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE,  
                     fill = ..level..,alpha=.5), bins = 7, 
                 geom = "polygon", data = dados2) +
  scale_alpha(guide="none") +
  scale_fill_gradient(low = "pink", high = "red", guide="none")+
  ggtitle("Mapa de Vitimas")

#MAPA DE ACIDENtES
ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE,  
                     alpha=..level..), bins = 9, 
                 geom = "polygon", data = dados) +
  scale_alpha(guide="none") +
  scale_fill_gradient(low = "#3ac4f9", high = "#3a40f9", guide="none")+
  ggtitle("Mapa de Acidentes")



#MAPA DE ACIDENtES
ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE,  
                     fill = ..level..,alpha=..level..), bins = 9, 
                 geom = "polygon", data = dados) +
  scale_alpha(guide="none") +
  scale_fill_gradient(low = "#3ac4f9", high = "#3a40f9", guide="none") +
  ggtitle("Mapa de Acidentes")


ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE,  
                     alpha=..level..), bins = 9, 
                 geom = "polygon", data = dados4) +
  scale_alpha(guide="none") +
  scale_fill_gradient(low = "pink", high = "red", guide="none")+
  ggtitle("Map")


# https://www.earthdatascience.org/tutorials/visualize-2d-point-density-ggmap/
ggmap(mh_map_set_dois) + 
  stat_density2d(aes(fill = ..level..), alpha = .5, 
                 h = .02, n = 300,
                 geom = "polygon", data = dados2) + 
  scale_fill_viridis() + 
  theme(legend.position = 'none')

#https://stackoverflow.com/questions/50551085/plotting-points-on-a-map-with-colors-depending-on-their-value-using-coordinates
ggmap(mh_map_set_dois) + geom_point(data = dados , aes(x=LATITUDE, y=LONGITUDE, color= TIPO_ACID, size = .5))


leaflet(dados2) %>%
  addTiles(group="OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addWebGLHeatmap(lng=~LONGITUDE, lat=~LATITUDE,  size=60000) %>%
  addCircles(~LONGITUDE, ~LATITUDE, popup=~paste("ID: ", ID,  "Local: ", LOCAL,   "<br>Ano: ", ANO, 
                                                 "<br>Tipo: ", TIPO_ACID, "<BR>N Feridos: ", FERIDOS, "<BR>N Feridos GRAVES: ", 
                                                 FERIDOS_GR, "<br>Fatais", FATAIS, "<br>Local: ", LOCAL_VIA, sep = " "),  
             weight = 1, radius=10, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("topleft", colors= "#ffa500", labels=paste(min(dados$ANO), max(dados$ANO), sep = "-"), title="Acidentes:")



leaflet(dados3) %>%
  addTiles(group="OSM") %>%
  addHeatmap(group="heat", lng=dados3$LONGITUDE, lat=dados3$LATITUDE, max=1, blur = 50) %>%
  addCircles(~LONGITUDE, ~LATITUDE, popup=~paste("Local: ", LOCAL, "<br>Ano: ", ANO, 
                                                 "<br>Tipo: ", TIPO_ACID, "<BR>N Feridos: ", FERIDOS, "<BR>N Feridos GRAVES: ", 
                                                 FERIDOS_GR, "<br>Fatais", FATAIS, "<br>Local: ", LOCAL_VIA, sep = " "),  weight = 0.1, radius=50, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("topleft", colors= "#ffa500", labels=paste(min(dados3$ANO), "-", max(dados3$ANO), "<br>Acidentes:", length(dados$ANO),   "<br>Com Mortes:", length(dados3$ANO), "<br>Mortes:", sum(dados3$FATAIS), sep = " "), title="Acidentes Fatais:")
  

pal = colorNumeric(colorRamp(c('green', 'red')), polys_dat$density)

leaflet() %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite",
                   options = providerTileOptions(noWrap = TRUE,minZoom=9)) %>%
  addPolygons(data=dados2,color= ~pal(density), stroke = FALSE) %>%
  setMaxBounds(-0.715485, 51.252031, 0.514984, 51.745313) %>%

pal <- colorNumeric(
  palette = c("red", "pink", "blue", "green"),
  domain = dados3$ANO
)

leaflet(dados3) %>%
  addTiles(group="OSM") %>%
  addCircles(~LONGITUDE, ~LATITUDE, popup=~paste("Local: ", LOCAL, "<br>Ano: ", ANO, 
                                                 "<br>Tipo: ", TIPO_ACID, "<BR>N Feridos: ", FERIDOS, "<BR>N Feridos GRAVES: ", 
                                                 FERIDOS_GR, "<br>Fatais", FATAIS, "<br>Local: ", LOCAL_VIA, sep = " "),  weight = 0.1, radius=40, color = ~pal, stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomright", pal = ~pal, values = ~ANO, title="Acidentes:")



head(quakes)
leaflet(quakes) %>% addProviderTiles(providers$OpenStreetMap) %>%
  addWebGLHeatmap(lng=~long, lat=~lat, size = 60000)

leaflet(quakes) %>% addProviderTiles(providers$OpenStreetMap) %>%
  addWebGLHeatmap(lng=~long, lat=~lat, intensity = ~mag, size=60000)



mh_map_set_dois = get_googlemap(center = c(-51.227729, -30.030948), zoom = 15, source="osm",
                                color = "color",
                                source = "google",
                                maptype = "roadmap")

ggmap(mh_map_set_dois) + 
  labs(x="longitude", y="latitude") + 
  stat_density2d(data=dados3, aes(x=LONGITUDE, y=LATITUDE, alpha= ..level.., fill= ..level..), colour=FALSE,
                 geom="polygon", bins=30) + 
  scale_fill_gradientn(colours=c(rev(rainbow(100, start=0, end=.7)))) + scale_alpha(range=c(0,.8)) + 
  guides(alpha=FALSE,fill=FALSE)
