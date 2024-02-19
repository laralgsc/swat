#install.packages("RFmerge")
#if (!require(devtools)) install.packages("devtools")
#library(devtools)
#install_github("hzambran/RFmerge")

library(zoo)
library(hydroTSM)
library(hydroGOF)
library(sf)
library(RFmerge)
library(terra)
library(lubridate)
library(tidyterra)
library(rts)
library(tidyverse)
library(parallel)

past <- function(...) {
  paste(..., sep = "")
} # Modifica o default de paste para sep = ""

#Nao precisa projetar agora, a funcao nem funcionou com os dados projetados
#EPSG <- 31983 #Sirgas2000 UTM23S
#EPSG <- 32723 #WGS84 UTM23S

# Tabela com precipitacao diaria nas estacoes locais

station_files <- list.files(path = "/Users/laralagesanches/Documents/BolsaDTI/DataSetFarinha/3EditInputs",
                          pattern = "_pcp", full.names = T)

timelenght <- nrow(read.table(station_files[1], header = T))

stations_data <- tibble(.rows = timelenght)
i=1
for (i in i:length(station_files)){
  pcp <- tibble(read.table(station_files[i], header = T))
  colnames(pcp) <- str_extract(station_files[i], "[0-9]{8}")
  stations_data <- add_column(stations_data, pcp)
}

x <- read_csv("/Users/laralagesanches/Documents/BolsaDTI/ANAplugin/stations_comp.csv")
y <- read_csv("/Users/laralagesanches/Documents/BolsaDTI/ANAplugin/stations_incomp.csv")

stations_data <- x
stations_data <- cbind(x,y[-1])

stations_data <- stations_data[-1] %>% replace(. < 0, NA) %>% # troca -99 ou -1 por NA
  zoo(order.by = stations_data$date)

#subset <- stations_data[which(index(stations_data) == "1995-01-01"):which(index(stations_data) == "2014-12-31")]

# Tabela com metadados das estacoes

# se o arquivo de pcp for o txt
stations <- read.delim("/Users/laralagesanches/Documents/BolsaDTI/DataSetFarinha/3EditInputs/pcp.txt", sep = ",") %>% 
  st_as_sf(coords = c('LONG', 'LAT'), crs = 4326) %>% 
  st_transform(EPSG) %>% 
  mutate(NAME = str_extract(NAME, "[0-9]{8}"))

# se o arquivo for o mapa da estacoes da ANA
#st_layers("/Users/laralagesanches/Documents/BolsaDTI/farinha/estacoesANA.gpkg")
read_sf("/Users/laralagesanches/Documents/BolsaDTI/farinha/estacoesANA.gpkg", layer = "precipitation_ana_v10")
read_sf("/Users/laralagesanches/Documents/BolsaDTI/farinha/estacoesANA.gpkg", layer = "precipitation_ana_v10_incomp")

a <- read_sf("/Users/laralagesanches/Documents/BolsaDTI/farinha/estacoesANA.gpkg", layer = "precipitation_ana_v10")
b <- read_sf("/Users/laralagesanches/Documents/BolsaDTI/farinha/estacoesANA.gpkg", layer = "precipitation_ana_v10_incomp")

stations <- a
stations <- rbind(a, b[1:3,])

stations_metadata <- data.frame(ID = stations[["Codigo"]], 
                                lon = st_coordinates(stations)[, "X"], 
                                lat = st_coordinates(stations)[, "Y"])

# Raster de precipitacao diaria
# obs: me parece que os dados da ANA tambem estao em UTC

raster_files <- list.files(path = "/Users/laralagesanches/Documents/BolsaDTI/farinha/pcp/dataset-derived-near-surface-meteorological-variables-a7bec6a4-6cdf-4d2a-a9ae-8353702314e8",
                         full.names = T)
#basin <- st_read("/Users/laralagesanches/Documents/BolsaDTI/farinha-basin.gpkg") %>%
#  st_transform(4326)

extent <- c(-48, -46, -8, -6)

stack <- rast(raster_files) %>% 
  crop(vect(ext(extent), past("epsg:", "4326")))*3600 # convert from kg m-2 s-1 to mm in an hour

stackTS <- rts(stack, time(stack)) %>% 
  apply.daily(sum) # convert from hourly to daily

names(stackTS@raster) <- as.Date(.indexDate(stackTS@time)-1) # precisei colocar -1 porque deu alguma confusao na timezone depois de apply.daily

#stackDay <- project(stackTS@raster, past("epsg:", EPSG), method = "bilinear") # nao precisa projetar agora, a funcao nem funcionou com os dados projetados
stackDay <- stackTS@raster

stackDay <- project(stackTS@raster, chirps, method = "near")

#writeRaster(stackDay, "/Users/laralagesanches/Documents/BolsaDTI/Interpolation/stackDay.tif", overwrite = T)
#stackDay <- rast("/Users/laralagesanches/Documents/BolsaDTI/Interpolation/stackDay.tif")

# DEM

DEM <- rast("/Users/laralagesanches/Documents/BolsaDTI/ASurf_WFDE5_CRU_v2.1.nc") %>% 
  crop(vect(ext(extent), past("epsg:", "4326")))

DEM <- list.files(file.path("/Users/laralagesanches/Documents/BolsaDTI/farinha", DEM_folder), pattern = ".hgt", full.names = T) %>% 
  sprc() %>% 
  merge() %>% 
  crop(vect(ext(extent))) %>% 
  project(chirps25, method = "bilinear")

# Mascara

mask <- vect(ext(extent), past("epsg:", "4326")) # na pratica nao e uma mascara

#### INTERPOLATION ####

a <- stations_data[1:3653]
b <- stackDay[[1:3653]]
cov <- list(wfde5 = b, dem = DEM)

rfmep <- RFmerge(x = stations_data[7306:9131], # class = zoo
                 metadata = stations_metadata, # class = data.frame
                 cov = list(chirps = chirps[[7306:9131]], wfde5 = stackDay[[7306:9131]], dem = DEM), 
                 mask = mask, # class = sf
                 training = 0.70, 
                 id = "ID", lat = "lat", lon = "lon", 
                 parallel = "none")

#pcp95a04 <- rfmep #1:3653
#pcp05a14 <- rfmep #3654:7305
#pcp15a19 <- rfmep #7306:9131

pcp <- rast(list(c(pcp95a04, pcp05a14, pcp15a19)))

#writeRaster(pcp, "/Users/laralagesanches/Documents/BolsaDTI/Interpolation/pcp_merged_chirps_wfde5.tif")

library(chirps)

#extent <- c(-47.5, -46.25, -7.25, -6.5)
#mask <- vect(ext(extent), past("epsg:", "4326"))

tictoc::tic()
chirps25 <- get_chirps(mask, 
                     c("2019-12-29","2019-12-31"), 
                     server = "CHC",
                     as.raster = T, 
                     resolution = 0.25)
tictoc::toc()

subbasin <- st_read("/Users/laralagesanches/Documents/BolsaDTI/farinha/delineatedwatershed.gpkg") %>%
  st_transform(4326)

plot(chirps25[[1]])
plot(subbasin, col = NA, add = T)
plot(mask, col = NA, add = T)

plot(stations, add = T)

writeRaster(chirps25, "/Users/laralagesanches/Documents/BolsaDTI/Interpolation/chirps_15_19.tif")

chirps95a04 <- rast("/Users/laralagesanches/Documents/BolsaDTI/Interpolation/chirps_95_04.tif")
chirps05a14 <- rast("/Users/laralagesanches/Documents/BolsaDTI/Interpolation/chirps_05_14.tif")
chirps15a19 <- rast("/Users/laralagesanches/Documents/BolsaDTI/Interpolation/chirps_15_19.tif")

chirps <- c(chirps95a04,chirps05a14, chirps15a19)
