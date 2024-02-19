library(tidyverse)
library(terra)
library(sf)
#library(tidyterra)

past <- function(...) {
  paste(..., sep = "")
} # Modifica o default de paste para sep = ""

## Variaveis
dir <- "/Users/laralagesanches/Documents/BolsaDTI"; setwd(dir); dir()
folder_from <- "farinha"
folder_to <- "DataSetFarinha"
  subfolders <- c("0DatabaseLara", "1DelineateWatershed", "2CreateHRUs", "3EditInputs", "4Calibration")

DEM_folder <- "SRTMoriginal"
basin_file <- "farinha-basin.gpkg"
lulc_file <- "mapbiomas-brazil-collection-71-cerrado-2021.tif"
soils_file <- "pedo_area.shp"
EPSG <- 31983 #Sirgas2000 UTM23S

## Pre processing
file.path(folder_to, subfolders) %>% sapply(dir.create, recursive = T) #Cria minhas pastas de input do modelo
dir.create(file.path(dir, folder_to, subfolders[4], "WGEN"))
basin <- st_read(basin_file) %>%
  st_transform(EPSG) %>% 
  st_buffer(1000)

######################################## DEM ########################################

DEM <- list.files(file.path(folder_from, DEM_folder), pattern = ".hgt", full.names = T) %>% 
  sprc() %>% 
  merge() %>% 
  project(past("epsg:", EPSG), method = "bilinear", res = 30) %>% 
  crop(basin, mask = T) %>% 
  writeRaster(file.path(folder_to, subfolders[2], "SRTM1arc_proj.tif"), overwrite = T)
#plot(DEM)

#DEM <- rast(file.path(folder_to, subfolders[2], "SRTM1arc_proj.tif"))

######################################## LULC ########################################

# Mapa
LULC <- rast(file.path(folder_from, lulc_file)) %>% 
  project(DEM, method = "near") %>% 
  crop(basin, mask = T) %>% 
  writeRaster(file.path(folder_to, subfolders[3], "LULC.tif"), overwrite = T)
#plot(LULC)
#freq(LULC)

# Lookup table
unique(LULC, na.rm = T) %>% 
  rename(LANDUSE_ID = 1) %>% 
  mutate(SWAT_CODE = NA) %>% 
  write_csv(file.path(folder_to, subfolders[3], "lulc_code.csv"), na = "")

#LULC[is.na(LULC)] <- -1
#LULC[LULC==0] <- NA
#while (freq(LULC, value = NA) != 0) {
#LULC = focal(LULC, matrix(1,3,3), fun = modal, na.rm = T, pad = T, NAonly = T)
#}
#LULC[LULC == -1] <- NA
#writeRaster(LULC, file.path(paste(dir, "LULC_0rm.tif", sep = "")), overwrite = T)

#LULC <- rast(file.path(folder_to, subfolders[3], "LULC.tif"))

######################################## SOILS ########################################

# Mapa
soils <- st_read(soils_file) %>% 
  st_transform(EPSG) %>% 
  st_intersection(basin) %>% 
  mutate(cod_unidad = as.numeric(factor(nom_unidad)))

# Lookup table
soils %>%
  st_drop_geometry() %>% 
  dplyr::select(SOIL_ID = cod_unidad, SNAM = nom_unidad) %>% 
  mutate(SNAM = str_remove_all(SNAM, "[:digit:]")) %>% 
  arrange(SOIL_ID) %>% 
  unique() %>%
  na.omit() %>% 
  write_csv(file.path(folder_to, subfolders[3], "soils_code.csv"))

soils <- soils %>%
  rasterize(DEM, "cod_unidad") %>% 
  writeRaster(file.path(folder_to, subfolders[3], "soils.tif"), overwrite = T)
#plot(soils)
#freq(soils)

#soils <- rast(file.path(folder_to, subfolders[3], "soils.tif"))

####################################### CLIMATE #######################################
library(tidyverse)
library(terra)
library(sf)
library(mgsub)

# TODAS AS ESTACOES PRECISAM TER A MESMA DATA DE INICIO E DE FIM, TEM QUE AJUSTAR O SCRIPT

## Functions
past <- function(...) {
  paste(..., sep = "")
} # Modifica o default de paste para sep = ""
rename.stack <- function(raster_path_list, stack){
  assign("element", NULL)
  assign("date_time", NULL)
  i = 1
  for (i in i:length(raster_path_list)) {
    element <- grep("GRIB_ELEMENT=", describe(raster_path_list[i]), value = T) %>% 
      str_remove("    GRIB_ELEMENT=") %>% 
      append(element, after = 0)
    date_time <- grep(" REF_TIME=", describe(raster_path_list[i]), value = T) %>% 
      str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z") %>% 
      append(date_time, after = 0)
  }
  names(stack) <<- paste(element, date_time, sep = "_")
}
gridpts.near <- function(stack, subbasins) {
  gridpts <- as.points(stack[[1]], values = F, na.all = T) %>% st_as_sf()
  gridpts$yx <- past(str_pad(gsub('-|\\.', '', format(yFromCell(stack[[1]], 1:ncell(stack[[1]])), nsmall = 1, digits = 2)),
                             width = 3, side = "left", pad = "0"), "S",
                     str_pad(gsub('-|\\.', '', format(xFromCell(stack[[1]], 1:ncell(stack[[1]])), nsmall = 1, digits = 2)),
                             width = 3, side = "left", pad = "0"), "W")
  return(gridpts[unique(st_nearest_feature(subbasins, gridpts)),]) # Apaga pontos inuteis
}
data.extract <- function(stack, gridpts_near) {
  data <- terra::extract(stack, gridpts_near, method = "simple", xy = T, ID = F)
  data$y <- past(str_pad(gsub('-|\\.', '', format(round(data$y, digits = 1),
                                                  nsmall = 1, 
                                                  digits = 2)),
                         width = 3, side = "left", pad = "0"), "S")
  data$x <- past(str_pad(gsub('-|\\.', '', format(round(data$x, digits = 1),
                                                  nsmall = 1, 
                                                  digits = 2)),
                         width = 3, side = "left", pad = "0"), "W")
  data <- unite(data, "yx", y:x, sep = "") %>% 
    data.table::transpose(keep.names = "id", make.names = "yx")
  return(data)
}
wrt.txt <- function(data, dir, variable) {
  if (is_tibble(data)) {
    i=2
    for (i in i:length(data)) {
      c(str_remove_all(pull(data[1,1]), "-"),
        format(round(pull(data[i]), digits = 3), nsmall = 3, trim = T)) %>% 
        as_tibble() %>% 
        write_delim(file.path(dir, past(names(data[i]), variable, ".txt")), delim = ",", col_names = F)
    }
  }
  if (!is_tibble(data)) {
    i = 2
    for (i in i:length(data[[1]])) {
      ma <-  c(format(data$Tmax_day[[1]][1], "%Y%m%d"),
               format(round(pull(data$Tmax_day[i]), digits = 3), nsmall = 3, trim = T))
      mi <-  c("",
               format(round(pull(data$Tmin_day[i]), digits = 3), nsmall = 3, trim = T))
      
      tibble(ma, mi) %>% 
        write_delim(file.path(dir, past(names(data$Tmax_day[i]), variable, ".txt")), delim = ",", col_names = F)
    }
  }
}

# Funcao para salvar um arquivo txt para cada estacao no formato requerido pelo SWAT,
# para funcionar a tabela de entrada deve ter a primeira coluna como data de cada registro e as demais colunas os registros de cada estacao
# Definicao de cada argumento:
# data = tabela com data e dados das estacoes
# dir = diretorio onde salvar
# variable.initial = texto padrao do SWAT para a variavel em questao

## Variaveis
dir <- "/Users/laralagesanches/Documents/BolsaDTI"; setwd(dir); dir()
folder_from <- "farinha"
folder_to <- "DataSetFarinha"
  subfolders <- c("0DatabaseLara", "1DelineateWatershed", "2CreateHRUs", "3EditInputs", "4Calibration")

dir_clim <- "/Users/laralagesanches/Documents/BolsaDTI/farinha/climate"; dir(dir_clim)
subbasins_file <- "/Users/laralagesanches/Documents/BolsaDTI/farinha/delineatedwatershed.gpkg"
acronyms <- tibble(products_id = c("rad", "rh", "tmp", "wnd"), #Name of climate variable as written in dir(dir_clim)
                   stations_file = c("slr", "hmd", "tmp", "wnd"), #According to SWAT output format
                   data_files = c("_slr", "_dwp", "_tmp", "_wnd")) #According to WGEN default

# swat_files <- c(".pcp",".tmp",".slr",".wnd", ".hmd")

EPSG <- 4326 #WGS84

DEM_folder <- "SRTMoriginal"
DEM <- list.files(file.path(folder_from, DEM_folder), pattern = ".hgt", full.names = T) %>% 
  sprc() %>% 
  merge()

## Pre processing
subbasins <- st_read(subbasins_file) %>% 
  st_transform(EPSG)
#plot(subbasins[3])
#st_crs(subbasins) == st_crs(stack[[2]])

#Talvez seja importante, parece que converte o horario para o utm local, mas nao utilizei
#with_tz(as.POSIXct(284040000, origin = "1970-01-01", tz = "UTC"), Sys.timezone()) 

#### Grided meteorological data to table ####
for (i in 1:4) {
  product <- acronyms$products_id[i]
  raster_list <- list.files(path = file.path(dir_clim, past(product, c("_CFSR", "_CFSv2"))),
                            pattern = '.grb2',
                            full.names = T)
  t <- tibble(raster_list, date = parse_date(sub(".*(\\d{6}).*", "\\1", raster_list), "%Y%m")) %>% 
    arrange(date) # Cria tabela c/ nomes dos arquivos, data referente a cada um e organiza em ordem cronologica
  
  ## Reprojection (same variables may have different resolutions depending on time series)
  stack_v1 <- rast(grep('CFSR', t$raster_list, value = T)) %>% 
    project(past("EPSG:", EPSG), method = "bilinear")
  stack_v2 <- rast(grep('CFSv2', t$raster_list, value = T)) %>% 
    project(stack_v1, method = "bilinear")
  stack <- terra::crop(c(stack_v1, stack_v2), subbasins, snap = "out"); rm(stack_v1, stack_v2)
  
  ## Grid points relevant to the watershed
  gridpts_near <- gridpts.near(stack, subbasins) %>% 
    st_write(dsn = file.path(dir_clim, "gridpts.gpkg"), layer = product, append = T)
  
  ## Stations description file
  tibble(ID = seq(1:length(gridpts_near$yx)),
         NAME = past(gridpts_near$yx, acronyms$data_files[i]),
         LAT = format(st_coordinates(gridpts_near)[,2], nsmall = 4, digits = 5),
         LONG = format(st_coordinates(gridpts_near)[,1], nsmall = 4, digits = 5),
         ELEVATION = terra::extract(DEM, gridpts_near, method = "simple", ID = F)[[1]]) %>% 
    write_delim(file.path(folder_to, subfolders[4], past(acronyms$stations_file[i], ".txt")), delim = ",", col_names = T)
  
  ## Data extraction
  rename.stack(t$raster_list, stack) # Necessario para identificacao nas proximas etapas
  data <- data.extract(stack, gridpts_near[7:8,])
  
  ## Object assignment
  assign(past(product, "_stack"), stack); rm(stack)
  assign(past(product, "_data"), data); rm(data)
}

#product <- acronyms$products_id[4]
#gridpts_near <- st_read(file.path(dir_clim, "gridpts.gpkg"), layer = product)
#plot(get(past(product, "_stack"))[[2]]); plot(subbasins$geom, add = T); plot(gridpts_near, add = T, col = "red")
#View(get(past(product, "_data")))

#### Raw data formatting ####
#Regarding processing, we simply took every hour's data and
#added it to get total rainfall,
#max/min temperature,
#mean RH, windspeed, and
#sum for SR.

#### Radiation ####
#(sum of 6h averages) * (seconds in a 6h period)/1000000
RAD_day <- rad_data %>% 
  mutate(date = as.Date(str_remove(id, ".*_")), .after = 1) %>% 
  group_by(date) %>% 
  summarise(across(where(is.double), sum)) %>% 
  mutate_if(is.numeric, ~.x * 21600/1000000)

wrt.txt(data = RAD_day,
        dir = file.path(dir, folder_to, subfolders[4]),
        variable = acronyms$data_files[1])

#plot
RAD_day %>% 
  group_by(date = year(date)) %>% 
  summarise(across(2:8, sum)) %>% 
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, color = name)) + 
  geom_line() +
  scale_x_continuous(limits = c(NA,2011))

#### Relative humidity ####
RH_day <- rh_data %>% 
  mutate(date = as.Date(str_remove(id, ".*_")), .after = 1) %>% 
  group_by(date) %>% 
  summarise(across(where(is.double), mean)) %>% 
  mutate_if(is.numeric, ~.x * 0.01)

wrt.txt(data = RH_day,
        dir = file.path(dir, folder_to, subfolders[4]),
        variable = acronyms$data_files[2])

#### Temperature ####
#TMAX
Tmax_day <- tmp_data %>% 
  filter(grepl("TMAX", id)) %>% 
  mutate(date = as.Date(str_remove(id, ".*_")), .after = 1) %>% 
  group_by(date) %>% 
  summarise(across(where(is.double), max))

#TMIN
Tmin_day <- tmp_data %>% 
  filter(grepl("TMIN", id)) %>% 
  mutate(date = as.Date(str_remove(id, ".*_")), .after = 1) %>% 
  group_by(date) %>% 
  summarise(across(where(is.double), min))

wrt.txt(data = list(Tmax_day = Tmax_day, Tmin_day = Tmin_day),
        dir = file.path(dir, folder_to, subfolders[4]),
        variable = acronyms$data_files[3])

#### Wind speed from u and v components ####
u_comp <- filter(wnd_data, grepl("UGRD", id))
v_comp <- filter(wnd_data, grepl("VGRD", id))

ws <- sqrt(as.matrix(u_comp[2:length(u_comp)])^2 + as.matrix(v_comp[2:length(u_comp)])^2)
rownames(ws) <- gsub("UGRD_", "", u_comp$id)

# Wind speed by date at 1.7m
WND_day <- as_tibble(ws, rownames = "id") %>% 
  mutate(date = as.Date(id)) %>% 
  group_by(date) %>% 
  summarise(across(2:length(u_comp), mean)) %>% 
  mutate(across(2:length(u_comp), ~ .x * (1.7/10)^0.2))

wrt.txt(data = WND_day,
        dir = file.path(dir, folder_to, subfolders[4]),
        variable = acronyms$data_files[4])

#### Precipitation ####

dir = "/Users/laralagesanches/Documents/BolsaDTI/ANAplugin/plu"

txt <- list.files(path = dir, pattern = "([0-9]).txt", full.names = T)

startdate <- as.Date(paste(read.table(txt[1])[1,1], past("0", read.table(txt[1])[1,2]), read.table(txt[1])[1,3], sep = "/"), format = "%e/%m/%Y")

stations <- tibble(.rows = nrow(read.table(txt[1])))
i=1
for (i in i:length(txt)){
  data <- tibble(read.table(txt[i])[[4]])
  colnames(data) <- parse_number(txt[i])
  stations <- add_column(stations, data)
} # str(stations) se alguma coluna vier com classe character provavelmente tem alguma virgula no arquivo por erro de formatacao

stations <- stations %>% 
  select(which(colSums(stations) != -nrow(stations))) %>% # seleciona apenas as colunas que tem observacao
  mutate(date = seq.Date(startdate, by = "day", length.out = nrow(stations)), 
         .before = 1)

stations %>% 
  reshape2::melt(id.vars = 'date', variable.name = 'station') %>% 
  ggplot(aes(date, y = value, color = station)) +
  geom_line() +
  scale_x_date(limits = c(startdate,NA)) +
  facet_wrap(vars(station), nrow = 4) +
  theme(legend.position="none")

good_stations <- stations %>% 
  select(date, contains(names(stations[c(5:12, 16:21, 24:27)])))
  
good_stations %>% 
  reshape2::melt(id.vars = 'date', variable.name = 'station') %>% 
  ggplot(aes(date, y = value, color = station)) +
  geom_line() +
  scale_x_date(limits = c(startdate, as.Date("2015-01-01"))) +
  facet_wrap(vars(station), nrow = 4) +
  theme(legend.position="none")

#write_csv(good_stations, "/Users/laralagesanches/Documents/BolsaDTI/ANAplugin/stations.csv", col_names = T, quote = "none")

#data <- read_table(unz("/Users/laralanches/Documents/southAmerica.zip", "55081_2022-02-01-02-54-17/s-170-434.txt"))
#data <- read_table(unzip("/Users/laralanches/Documents/southAmerica.zip", files[2], exdir = "/Users/laralanches/Documents"))


## Grid points relevant to the watershed

pcp <- rast("/Users/laralagesanches/Documents/BolsaDTI/Interpolation/pcp_merged_chirps_wfde5.tif")

gridpts_near <- gridpts.near(pcp, subbasins) %>% 
  st_write(dsn = file.path(dir_clim, "gridpts.gpkg"), layer = "pcp", append = T)

## Stations description file
tibble(ID = seq(1:length(gridpts_near$yx)),
       NAME = past(gridpts_near$yx, "_pcp"),
       LAT = format(st_coordinates(gridpts_near)[,2], nsmall = 4, digits = 5),
       LONG = format(st_coordinates(gridpts_near)[,1], nsmall = 4, digits = 5),
       ELEVATION = terra::extract(DEM, gridpts_near, method = "simple", ID = F)[[1]]) %>% 
  write_delim(file.path(folder_to, subfolders[4], past("pcp", ".txt")), delim = ",", col_names = T)

## Data extraction
data2 <- data.extract(pcp, gridpts_near)

write_delim(data2, "/Users/laralagesanches/Documents/BolsaDTI/Interpolation/pcp_merged_chirps_wfde5.txt", 
            na = "", delim = ",", quote = "none", col_names = T)

wrt.txt(data = tibble(read.table("/Users/laralagesanches/Documents/BolsaDTI/Interpolation/pcp_merged_chirps_wfde5_2.txt", header = T)),
        dir = file.path(dir, folder_to, subfolders[4]),
        variable = "_pcp")

######################################## WGEN ########################################

# Text data files must have:
# The word Text on line 1 (make sure the T is capitalized)
# The start date on line 2
# The end date on line 3

## Variables
files <- list.files(file.path(dir, folder_to, subfolders[4]), pattern = "_", full.names = T)  
variables <- c("pcp","tmp","slr","wnd","dwp", "hhr")
    #"Note on Dew Point: For the 2009 version of SWAT, relative humidity may be used instead, if it is entered as a decimal less than 1."
    #"Note on Maximum Half Hour Rainfall: This is not one of the usual weather input files needed to run SWAT, but it is required for the weather generator."

## Pre processing
stations <- list.files(file.path(dir, folder_to, subfolders[4]), pattern = '_', full.names = F) %>% 
  str_extract(".*_") %>% 
  unique()

i = 1
for (i in 1:length(files)) {
  file <- read_delim(files[i], delim = ",", col_types = "c")
  
  start_date <- as.Date(names(file)[1], "%Y%m%d") %>% 
    format("%m/%d/%Y")
  end_date <- (as.Date(names(file)[1], "%Y%m%d") + nrow(file) - 1) %>% 
    format("%m/%d/%Y")

   file %>% 
    add_row(tibble(!!names(file)[1] := c("Text", start_date, end_date)), .before = 1) %>% 
    write_delim(sub(file.path(subfolders[4], ""), file.path(subfolders[4], "WGEN", ""), files[i]), 
                na = "", delim = ",", quote = "none", col_names = F)
} # Formata arquivos com dados para o WGEN

i=1
for (i in 1:length(stations)) {
  j=1
  for(j in 1:length(variables)) {
    write_delim(tibble(NA), 
                file.path(dir, folder_to, subfolders[4], "WGEN", 
                          past(stations[i], variables[j], ".txt")), 
                delim = ",", append = T, na = "", eol = "")
  }
} # Cria arquivos vazios de estacoes sem dados
