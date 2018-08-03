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
filenames <- list.files(path = "/Users/fsmoura/Desktop/docs/geo/") 
data <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
data = read.csv("acidentes-2016 (1).csv", header = TRUE, sep = ";")
dim(data)
#dados <- data[data$LOG1 == "AV PROTASIO ALVES", ]
dados <- data
dim(dados)
mh_map_set_dois = get_googlemap(center = c(-51.200421939611394, -30.06753685571282), zoom = 12, source="osm",
color = "color",
source = "google",
maptype = "roadmap")
mapPoints <- ggmap(mh_map_set_dois)
mapPoints
names(data)
dados <- cbind(data$long, data$lat, data$default, data$tipo)
dados <- as.data.frame(dados)
dados$V1 <- as.numeric(as.character(dados$V1))
dados$V2 <- as.numeric(as.character(dados$V2))
dados <- dados[dados$V2 < 0, ]
dados <- subset(dados, !is.na(V1))
dim(dados)
dados$newrow <- sample(1000, size = nrow(dados), replace = TRUE)
dados$cut <- cut(dados$newrow, breaks=seq(0,1000,200), labels=sprintf("Score %d-%d",seq(0, 800, 200), seq(200,1000,200)))

##Mapa de pontos
mapPoints <- ggmap(mh_map_set_dois) + 
  geom_point(data = dados[1,], aes(x = V1, y = V2), 
             fill = "red", alpha =0.2, size = 0.5, shape = 21,stroke = 0) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("EPTC - Alvarás 2017")
mapPoints


#Mapa de densidade amarelo / vermelho + linhas
ggmap(mh_map_set_dois) +
  geom_density2d(data=dados,aes(x=V1,y=V2), bins=20) +
  stat_density2d(data=dados,aes(x=V1,y=V2,fill=..level.., alpha=..level..), geom='polygon')+ 
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x = "Longitude", y = "Latitude") + ggtitle("Densidade de Alvarás 2017 - EPTC")



##Mapa de densidade verde/vermelho
ggmap(mh_map_set_dois, extent = "device") + 
  geom_density_2d(data = dados,aes(x = V1, y = V1), 
                 size = 0.3) + 
  stat_density_2d(data = dados, 
                 aes(x = V1, y = V2, fill = ..level.., alpha = ..level..), size = 0.01, 
                bins = 200, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) 


## scale_colour_hue
ggmap(mh_map_set_dois) + 
  geom_point(data = dados, aes(x = V1, y = V2, colour = cut), alpha = 0.2) + 
  scale_colour_hue(h = c(180, 270)) +
  ggtitle("...") +
  xlab("...") + ylab("...") 

## Classe por Cor
#https://stablemarkets.wordpress.com/2015/03/07/vehicle-accident-density-in-nyc/
col_vals=c('Score 0-200'='dodgerblue', 'Score 200-400'='darkred',
           'Score 400-600'='violet','Score 600-800'='darkgreen',
           'Score 800-1000'='darkgoldenrod4')
ggmap(mh_map_set_dois) +
  stat_density2d(data=dados, geom='polygon',bins = 10, aes(x=V1,y=V2,fill = cut, alpha=..level..))+
  scale_fill_manual(values=col_vals)+
  #guides(fill = guide_colorbar(barwidth = 1, barheight = 12)) +
  scale_alpha(guide = FALSE)+ 
  xlab(' ')+ylab(' ')+
  ggtitle('Classe por cor')

ggmap(mh_map_set_dois) +
  stat_density2d(data= dados, aes(x = V1, y = V2, alpha=.75,fill=..level..),bins = 10, geom = 'polygon')+
  guides(fill = guide_colorbar(barwidth = 1, barheight = 12)) +
  scale_alpha(guide = FALSE)+ 
  xlab(' ')+ylab(' ')+
  ggtitle('Densidade de Alvarás')




#########################
##Mapa de Pontos Red
ggmap(mh_map_set_dois, extent='device') +
  geom_point(aes(x=V1, y=V2), colour="red", alpha=0.1, size=0.1, data=dados)


ggmap(mh_map_set_dois, extent='device') +
  geom_point(aes(x=V1, y=V2, colour = V4), alpha=0.1, size=1, data=dados)


###############################
#Densidade red/yellow
ggmap(mh_map_set_dois) +
  #geom_density2d(data=dados, aes(x=V1, y=V2), size=.1) +
  stat_density2d(data=dados, aes(x=V1, y=V2,  fill = ..level.., alpha = ..level..), size = 0.2, bins = 250, geom = 'polygon')+
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE) + 
  ggtitle("EPTC Pontos Comerciais")

##
#https://nycdatascience.com/blog/student-works/nyc-speed-camera-program-revenue-or-safety-well-get-you-up-to-speed-in-a-flash/
ggmap(mh_map_set_dois) +
  #geom_density2d(data=dados, aes(x=V1, y=V2), size=.1) +
  stat_density2d(data=dados, aes(x=V1, y=V2,  fill = ..level.., alpha = ..level..), 
    bins = 250, geom = 'polygon')+
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE) + 
  geom_point(aes(x=V1, y=V2, size=(V4/100)), color='green', data=dados[1:10,]) + 
  scale_size_continuous(range=c(1,3))+
  ggtitle("EPTC Pontos Comerciais")

#Densidade blue/red
ggmap(mh_map_set_dois) +
  stat_density2d(data = dados, aes(x = V1, y = V2, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.1, bins = 9) +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_alpha(range = c(0, 0.9), guide = FALSE)


ggmap(mh_map_set_dois) %+% dados +
  aes(x = V1, y = V2, z = newrow) +
  stat_summary_2d(fun = median, binwidth = c(.3, .3), alpha = 0.5) +
  scale_fill_gradientn(name = "Median", colours = terrain.colors(10), space = "Lab") +
  labs(x = "Longitude", y = "Latitude") +
  coord_map()
  



## LM
ggmap(mh_map_set_dois) + 
  stat_density2d(data = dados, aes(x = V1, y = V2, fill = ..level.., alpha = ..level..),
                                        geom = "polygon", size = 0.01, bins = 10) +
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()



##
ggmap(mh_map_set_dois, extent='device', legend='topleft')+
  geom_point(aes(x=V1, y=V2, size=V4), color='red', data=dados[1:10,]) + 
  scale_size_continuous(range=c(2,8))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.title.x=element_blank(),axis.title.y=element_blank()) +
  stat_density2d(aes(x=V1, y=V2, fill = ..level.., alpha=..level..), 
                 size = 5, bins=120, geom='polygon', data=dados) +
  scale_alpha(range = c(0,0.35), guide=FALSE) + 
  scale_fill_gradient(limits = c(200,10000), low='yellow', high='orange') +
  labs(title = 'Densidade', size='Indice', fill = 'fill')



#https://stackoverflow.com/questions/32148564/heatmap-plot-by-value-using-ggmap


gg <- ggmap(mh_map_set_dois)
gg <- gg + stat_density2d(data=dados, aes(x=V1, y=V2, colours= cut, fill=..level.., alpha=..level..),
                          geom="polygon", size=0.01, bins=120)
gg <- gg + scale_fill_viridis()
gg <- gg + scale_alpha(range=c(0.2, 0.4), guide=FALSE)
gg <- gg + coord_map()
gg <- gg + facet_wrap(~cut, ncol=2)
gg <- gg + labs(x=NULL, y=NULL, title="Score Distribution Across All Schools\n")
gg <- gg + theme_map(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(face="bold", hjust=1))
gg <- gg + theme(panel.margin.x=unit(1, "cm"))
gg <- gg + theme(panel.margin.y=unit(1, "cm"))
gg <- gg + theme(legend.position="right")
gg <- gg + theme(strip.background=element_rect(fill="white", color="white"))
gg <- gg + theme(strip.text=element_text(face="bold", hjust=0))
gg


dados$cut <- cut(dados$newrow, breaks=seq(0,1000,500), labels=sprintf("Score %d-%d",seq(0, 500, 500), seq(500,1000,500)))
gg <- ggmap(mh_map_set_dois)
gg <- gg + stat_density2d(data=dados[1:20000], aes(x=V1, y=V2,  fill=..level.., alpha=..level..),
                          geom="polygon", size=0.01, bins=120)
gg <- gg + scale_fill_viridis()
gg <- gg + scale_alpha(range=c(0.2, 0.4), guide=FALSE)
gg <- gg + coord_map()
#gg <- gg + facet_wrap(~cut, ncol=3)
gg <- gg + scale_fill_gradient(low = "green", high = "red")
gg <- gg + labs(x=NULL, y=NULL, title="Score Distribution Across All Schools\n")
gg <- gg + theme_map(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(face="bold", hjust=1))
gg <- gg + theme(panel.margin.x=unit(1, "cm"))
gg <- gg + theme(panel.margin.y=unit(1, "cm"))
gg <- gg + theme(legend.position="right")
gg <- gg + theme(strip.background=element_rect(fill="white", color="white"))
gg <- gg + theme(strip.text=element_text(face="bold", hjust=0))
gg



library(RColorBrewer)
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
ggmap(mh_map_set_dois) +
  stat_density2d(data = dados[1:50000,], aes(x = V1, y = V2, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01, bins = 160) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE) 



ggmap(mh_map_set_dois) +
  stat_density2d(data = dados[1:50000,], aes(x = V1, y = V2, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01, bins = 16) +
  scale_fill_gradientn(name = "Median", colours = YlOrBr, space = "Lab") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)




gg <- ggmap(mh_map_set_dois)
gg <- gg + stat_density2d(data=dados[1:40000,], aes(x=V1, y=V2, 
                                                    fill=..level.., alpha=..level..),
                          geom="polygon", size=0.01, bins=14)
gg <- gg + scale_fill_viridis()
gg <- gg + scale_alpha(range=c(0.2, 0.4), guide=FALSE)
gg <- gg + scale_fill_gradient(low = "green", high = "red")
gg <- gg + scale_colour_gradient(low = "white", high = "green")
gg <- gg + coord_map()
#gg <- gg + facet_wrap(~cut, ncol=3)
gg <- gg + labs(x=NULL, y=NULL, title="Titulo\n")
gg <- gg + theme(panel.margin.x=unit(1, "cm"))
gg <- gg + theme(panel.margin.y=unit(1, "cm"))
gg <- gg + theme(legend.position="right")
gg <- gg + theme(strip.background=element_rect(fill="white", color="white"))
gg <- gg + theme(strip.text=element_text(face="bold", hjust=0))
gg


#scale_fill_gradientn(colours=c(rev(rainbow(100, start=0, end=.7))))
gg <- gg + scale_fill_viridis()
gg <- gg + scale_alpha(range=c(0.2, 0.4), guide=FALSE)
gg <- gg + scale_fill_gradientn(colours=c(rev(rainbow(100, start=0, end=.7))))
gg <- gg + coord_map()
#gg <- gg + facet_wrap(~cut, ncol=3)
gg <- gg + labs(x=NULL, y=NULL, title="Titulo\n")
gg <- gg + theme(panel.margin.x=unit(1, "cm"))
gg <- gg + theme(panel.margin.y=unit(1, "cm"))
gg <- gg + theme(legend.position="right")
gg <- gg + theme(strip.background=element_rect(fill="white", color="white"))
gg <- gg + theme(strip.text=element_text(face="bold", hjust=0))
gg

#http://data-analytics.net/cep/Schedule_files/geospatial.html
ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = V1, y = V2, fill = ..level..,alpha=..level..), bins = 100, geom = "polygon", data = dados) +
  scale_fill_gradient(low = "black", high = "red")+
  ggtitle("Map")


ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = V1, y = V2,  fill = ..level..,alpha=..level..), bins = 5, geom = "polygon", data = dados) +
  scale_fill_gradient(low = "black", high = "red")+
  ggtitle("Map")


#https://datascienceplus.com/visualising-thefts-using-heatmaps-in-ggplot2/
######**************************
ggmap(mh_map_set_dois) + 
  geom_tile(data = dados, aes(x = V1, y = V2, alpha = newrow),fill = 'red') +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())





#https://datascienceplus.com/visualising-thefts-using-heatmaps-in-ggplot2/
dados$newrow <- sample(100, size = nrow(dados), replace = FALSE)
ggmap(mh_map_set_dois) + 
  geom_point(data=dados, aes(x=V1, y=V2, color=newrow, size=newrow)) + 
  scale_color_gradient(low='yellow', high='red')





#https://datascienceplus.com/visualising-thefts-using-heatmaps-in-ggplot2/
ggmap(mh_map_set_dois) + 
  geom_tile(data = dados, aes(x = V1, y = V2, alpha = newrow, ),fill = 'red') +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())


library(splitstackshape)
dfexp <- expandRows(dados, "newrow")
ggmap(mh_map_set_dois)  %+% dados +
  aes(x = V1, y = V2) +
  stat_density_2d(aes(fill = ..level..), geom="polygon") +
  geom_point(position="jitter", alpha=.2, colour="white")  


ggmap(mh_map_set_dois)  %+% dados +
  aes(x = V1, y = V2) +
  stat_density_2d(aes(fill = ..level..), geom="polygon") +
  scale_colour_gradientn(colours=rainbow(4))



HoustonMap <- ggmap(mh_map_set_dois, extent = "device", legend = "topleft")
HoustonMap +
  stat_density2d(
    aes(x = V1, y = V2, fill = ..level..,
        alpha = ..level..),
    size = 2, bins = 6, data = dados,
    geom = "polygon")



#https://community.rstudio.com/t/plot-graphical-distribution-using-ggplot2-and-ggmap/4415/2
ggmap(mh_map_set_dois) + 
  geom_point(data=dados,aes(x = V1, y = V2,  z = newrow, fill = V1*V2)) + 
  geom_tile() + 
  coord_equal() +
  geom_point(data = dados, mapping = aes(V1, V2),shape=1, inherit.aes = FALSE)+
  scale_fill_distiller(palette="Spectral", na.value="white",limits=c(0,0.4)) + 
  scale_x_reverse()+
  theme_bw()+
  ggtitle("Chlorophyll-a distribution")+
  ylab("Latitude S") + xlab("Longitude E")+
  labs(fill = "Chl-a (mg/m3)")



#https://stackoverflow.com/questions/18285415/density2d-plot-using-another-variable-for-the-fill-similar-to-geom-tile

theme_set(theme_bw(base_size = 8))
colormap <- c("Violet","Blue","Green","Yellow","Red","White")
pred.stat.map <- ggmap(mh_map_set_dois) %+% dados + 
  aes(x = V1,
      y = V2,
      z = newrow) +
  stat_summary_2d(fun = median, 
                 binwidth = c(.001, .001),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "Tempo",
                       colours = colormap,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()
print(pred.stat.map)



colormap <- c("Violet","Blue","Green","Yellow","Red","White")
pred.stat.map <- ggmap(mh_map_set_dois) %+% dados + 
  aes(x = V1,
      y = V2,
      z = newrow) +
  stat_summary_2d(fun = median, 
                  binwidth = c(.01, .01),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Tempo",
                       colours = colormap,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()
print(pred.stat.map)





theme_set(theme_bw(base_size = 8))
pred.stat.map <- ggmap(mh_map_set_dois) %+% dados + 
  aes(x = V1,
      y = V2,
      z = newrow) +
  stat_summary_2d(fun = median, 
                 binwidth = c(.005, .005),
                 alpha = 0.5) + 
  scale_fill_gradient2(low = "yellow", mid = "white", high = "red") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()
print(pred.stat.map)



dados2 <- dados[1:50000,]
ggmap(mh_map_set_dois) + 
    labs(x="longitude", y="latitude") + 
    stat_density_2d(data=dados, aes(x=V1, y=V2, alpha= ..level.., fill= ..level..), colour=FALSE,
                   geom="polygon", bins=15) + 
    scale_fill_gradientn(colours=c(rev(rainbow(5, start=0, end=.7)))) + 
    scale_alpha(range=c(0,.8)) + 
    guides(alpha=FALSE,fill=FALSE)



library(leaflet.extras)
data <- read.csv("DATA.csv", sep=";")
data <- subset(data, !is.na(CrdLatDeg))
leaflet(data) %>%
    addTiles(group="OSM") %>%
    addHeatmap(group="heat", lng=~data$CrdLonDeg, lat=~data$CrdLatDeg, max=.6, blur = 60)


ggmap(mh_map_set_dois) + 
    geom_point(data = dados, aes(x = V1, y = V2, fill = dados$newrow, colour = "red"), 
               alpha = 0.1, size = 1, shape = 16) + 
               guides(fill=FALSE, alpha=FALSE, size=FALSE)




data <- read.csv("DATA.csv", sep=";")
data <- subset(data, !is.na(CrdLatDeg))
leaflet(dados) %>%
    addTiles(group="OSM") %>%
        addHeatmap(group="heat", lng=~V1, lat=~V2, max=.5, blur = 6)

######
leaflet(dados) %>%
    addTiles(group="OSM") %>%
    addHeatmap(group="heat", lng=dados$V1, lat=dados$V2, max=.5, blur = 60) %>% 
    addCircles(~V1, ~V2, popup=dados$newrow,  weight = 0.1, radius=1, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
    addLegend("bottomright", colors= "#ffa500", labels="Dunkin", title="In Connecticut")
    #%>%
    #addMarkers(~V1, ~V2, popup = ~as.character(newrow), label = ~as.character(newrow))


dados$LATITUDE <- dados$LATITUDE - 0.00090

leaflet(dados) %>%
    addTiles(group="OSM") %>%
    addCircles(~LONGITUDE, ~LATITUDE, popup=~LOCAL,  weight = 0.1, radius=4, color="red", stroke = TRUE, fillOpacity = 0.8) %>% 
    addLegend("bottomright", colors= "#ffa500", labels="Com??rcio e Servi??os", title="Alvaras de:")


dados3 <- dados[1:100,]

leaflet(dados) %>% addTiles() %>%
    addMarkers(~LONGITUDE, ~LATITUDE, popup =~LOCAL, label =~LOCAL)



leaflet(dados) %>% addTiles() %>%
    addCircles(~LONGITUDE, ~LATITUDE, popup=~newrow,  weight = 0.1, radius=40, color="#ffa500", stroke = TRUE, fillOpacity = 0.8)



#https://stackoverflow.com/questions/32275213/how-do-i-connect-two-coordinates-with-a-line-using-leaflet-in-r
mydf <- data.frame(Observation = c("A", "B","C","D","E"),
                   InitialLat = c(62.469722,48.0975,36.84,50.834194,50.834194),
                   InitialLong = c(6.187194, 16.3108,-2.435278,4.298361,4.298361),
                   NewLat = c(51.4749, 51.4882,50.861822,54.9756,54.9756),
                   NewLong = c(-0.221619, -0.302621,-0.083278,-1.62179,-1.62179),
                   stringsAsFactors = FALSE)

mydf
map3 = leaflet(dados) %>% addTiles()
map3 %>% addMarkers(~V1,~V2, popup=~cut)




map3 = leaflet(dados) %>% addTiles()
map3 %>% addMarkers(~V1,~V2, popup=~newrow)




library(ggplot2)
library(ggmap)
library(RColorBrewer)
drone.map <- ggmap(mh_map_set_dois, extent="device", legend="none")
drone.map <- drone.map + stat_density2d(data=dados,
  aes(x=V1, y=V2, fill=..level.., alpha=..level..), geom="polygon")
## Define the spectral colors to fill the density contours
drone.map <- drone.map + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
drone.map <- drone.map + geom_point(data=dados,
                                    aes(x=V1, y=V2), size=0.1, fill="red", shape=21, alpha=0.8)
## Remove any legends
drone.map <- drone.map + guides(size=FALSE, alpha = FALSE)
## Give the map a title
drone.map <- drone.map + ggtitle("US Drone Strikes in Pakistan from 2008 to 2013")
drone.map




#### The best
library(apcluster)
cl1 <- cbind(rnorm(30, 0.3, 0.05), rnorm(30, 0.7, 0.04))
cl2 <- cbind(rnorm(30, 0.7, 0.04), rnorm(30, 0.4, .05))
x1 <- rbind(cl1, cl2)
x1
plot(x1, xlab="", ylab="", pch=19, cex=0.8)
apres1a <- apcluster(negDistMat(r=2), x1)
s1 <- negDistMat(x1, r=2)
apres1b <- apcluster(s1)
apres1a
plot(apres1a, x1)
heatmap(apres1a)
heatmap(apres1b, s1)
apres1c <- apcluster(s1, details=TRUE)
plot(apres1c)
cl3 <- cbind(rnorm(20, 0.50, 0.03), rnorm(20, 0.72, 0.03))
cl4 <- cbind(rnorm(25, 0.50, 0.03), rnorm(25, 0.42, 0.04))
#x2 <- rbind(x1, cl3, cl4)
x2 <- rbind(x1, cl3, cl4)
x2
plot(x2, xlab="", ylab="", pch=19, cex=0.)
apres2a <- apcluster(negDistMat(r=2), x2)
plot(apres2a, x2)
apres2b <- apcluster(negDistMat(r=2), x2, q=0)
plot(apres2b, x2)
apres2c <- apcluster(negDistMat(r=2), x2, q=0.8)
plot(apres2c, x2)
apres2c@p     
heatmap(apres2c)     




x2 <- cbind(dados$V1, dados$V2, dados$cut)
x2 <- x2[complete.cases(x2), ]
head(x2)

x2 <- x2[sample(nrow(x2), 2500), ]
plot(x2, xlab="", ylab="", pch=19, cex=0.2)
apres2a <- apcluster(negDistMat(r=2), x2)
plot(apres2a, x2)
heatmap(apres2a)
apres2b <- apcluster(negDistMat(r=2), x2, q=0)
plot(apres2b, x2)
apres2c <- apcluster(negDistMat(r=2), x2, q=0.8)
plot(apres2c, x2)
apres2c@p     
heatmap(apres2c)  

save(apres2a, file = "mydata.rda")
load(file = "mydata.rda")
