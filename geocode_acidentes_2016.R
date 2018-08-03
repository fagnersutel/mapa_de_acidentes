setwd("/Users/fsmoura/Desktop/docs/")
library(ggmap)
       
#Carga de dados
origAddress <- read.csv("acidentes-2016 (1).csv", header = TRUE, sep = ";")
#Verificamos o tamanho do data set
dim(origAddress)
#View(origAddress)
#Removemos os duplicados
origAddress <- origAddress[!duplicated(origAddress[,1:2]), ]
#Verificamos o tamanho do data set
head(origAddress)
#criamos a variavel de pesquisa
origAddress$addresses <- paste(origAddress$LOG1, " and ", origAddress$LOG2, sep = "")
#agregamosao endereço o bairro e cidade
origAddress$addresses <- paste(origAddress$addresses,  "Porto Alegre", sep = " ")
origAddress$addresses <- paste(origAddress$addresses, "key=AIzaSyBA2Kt4jMYJe3up70J0Kl48Idrm69K6n0I", sep = " %26&")
#Eliminamos a virgula
origAddress$addresses <- gsub(",", "", origAddress$addresses)
#fazemos o tipecast
origAddress$addresses <- as.character(origAddress$geoAddress)
#verificamos o resultado final
head(origAddress)
dim(origAddress)
#definimoso range da pesquisa
origAddress<- origAddress[1:5,]
#fazemos a restricao
addresses <- origAddress$addresses
#verificamos o tamanho
length(addresses)
#executamosa funcao
getGeoDetails <- function(address){
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausando as:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(1)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  if (geo_reply$status != "OK"){
    answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=address, address_type=NA, status=NA)
    return(answer)
  }
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
 
  
  return(answer)
}
geocoded <- data.frame()

startindex <- 1

for (ii in seq(startindex, length(addresses))){
  print(paste("Trabalhando no item ", ii, " de", length(addresses)))

  result = getGeoDetails(addresses[ii]) 
  print(result$status)
  result$index <- ii
  result$tipo <-  origAddress$Atividade[ii]
  result$default <-  origAddress$Endereco[ii]


  geocoded <- rbind(geocoded, result)

}
head(geocoded)
#write.csv(geocoded, "saida2.csv", row.names=FALSE)

geocoded$address_type<-NULL
geocoded$status<-NULL
geocoded$lat <- gsub(" ", ".", geocoded$lat)
geocoded$long<- gsub(" ", ".", geocoded$long)

data <- paste("acid_", as.character(as.numeric(Sys.time())), ".csv", sep = "")
write.table(geocoded,file=data,sep=";",dec = " ", row.names=FALSE)
data <- paste("./geo/", data, sep = "")
write.table(geocoded,file=data,sep=";",dec = " ", row.names=FALSE)




