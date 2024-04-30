##################################### DIRETORIO E PACOTES #####################################

dir = "~/Documents/DadosEspaciais/"
dir2 = "~/Documents/DataSetPNCV/"
setwd(dir)
library(raster)
library(rgdal)
library(sf)
library(rmapshaper)
library(stars)
library(tmap)
library(fs)

# https://spatialreference.org/ref/epsg/ (31983 -> SIRGAS2000 UTM23S) ou (32723 -> WGS84 UTM23S)

###################################### LIMITES SUB-BACIAS ######################################

#dir()
#dir(paste(dir, "otto-PNCV", sep = ""))
otto = read_sf(paste(dir, "otto-PNCV/subbacias-PNCV.shp", sep = "")); otto
otto.or = otto

#### CRS

otto = st_transform(otto, 31983); otto

#### Dissolver

otto = ms_dissolve(otto)
#plot(otto)

#### Salvar

otto = as_Spatial(otto)
writeOGR(otto, dsn = dir2, driver = "ESRI Shapefile", "sub-baciasPNCV", overwrite = T)

#### Reabrir o arquivo da pasta local

#otto = read_sf(paste(dir2, "sub-bacia-buffer.shp", sep = "")); otto
#plot(otto)

############################################# DEM #############################################

#dir()
dem = raster("srtm-PNCV.tif"); dem

#### CRS

dem = projectRaster(dem, crs = 31983, method = "bilinear", res = 30); dem
#dem.proj = dem
#dem = dem.proj

#### Cortar pelos limites das sub bacias

dem = crop(dem, otto)
dem = mask(dem, otto)
#plot(dem)

#### Salvar

writeRaster(dem, file.path(dir2, "dem-baciaPNCV.tif"), overwrite = T)

#### Reabrir o arquivo da pasta local

#dem = raster(paste(dir2, "dem-PNCV.tif", sep = "")); dem
#plot(dem)

######################################### USO DA TERRA #########################################

#dir()
usoterra = raster("mapbio5-PNCV-2019.tif"); usoterra

#### CRS

usoterra = projectRaster(usoterra, crs = 31983, method = "ngb", res = 30); usoterra
#usoterra.proj = usoterra
#usoterra = usoterra.proj

#### Limites

usoterra = crop(usoterra, otto)
usoterra = mask(usoterra, otto)
#plot(usoterra)

#### Reclassificar valores das celulas* (se necessario)

#freq(usoterra)
#zero = usoterra %in% 33
#plot(zero)
#values(usoterra)[values(usoterra) == 0] = -99
#usoterra[is.na(usoterra)] = -99

#### Salvar

writeRaster(usoterra, file.path(dir2, "usoterra-bacia-PNCV-99.tif"), overwrite = T)

#### Reabrir o arquivo da pasta local

#usoterra = raster(paste(dir2, "usoterra-bacia-PNCV.tif", sep = "")); usoterra
#plot(usoterra)

######################################## USO DA TERRA 2 ########################################

#dir()
#dir(paste(dir, "usoterra", sep = ""))
usoterra = read_sf(paste(dir, "usoterra-DF-sirgas.shp", sep = "")); usoterra

# Novo shapefile a partir de um unico atributo

#View(usoterra)
#names(usoterra)
usoterra = subset(usoterra, select = "CODIGO")
usoterra = st_crop(usoterra, st_bbox(otto))
usoterra = ms_dissolve(usoterra, "CODIGO")
#plot(usoterra)

#### CRS

usoterra = st_transform(usoterra, 31983); usoterra

#### Vetor para raster

usoterra = as_Spatial(usoterra)
usoterra = rasterize(usoterra, solos, "CODIGO"); usoterra
#plot(usoterra)

#### Limites

usoterra = mask(usoterra, otto)
#plot(usoterra)
#usoterra.or = usoterra

#### Reclassificar valores das celulas (remover classe "outros" e "agua")

#freq(usoterra)
values(usoterra)[values(usoterra) == 77] = 29
values(usoterra)[values(usoterra) == 55] = 48
values(usoterra)[values(usoterra) == 42] = 43
values(usoterra)[values(usoterra) == 44] = 46
values(usoterra)[values(usoterra) == 50] = 59
values(usoterra)[values(usoterra) == 61] = 59
values(usoterra)[values(usoterra) == 62] = 60
values(usoterra)[values(usoterra) == 63] = 60
values(usoterra)[values(usoterra) == 64] = 60
values(usoterra)[values(usoterra) == 49] = 71
values(usoterra)[values(usoterra) == 69] = 71
values(usoterra)[values(usoterra) == 70] = 71
values(usoterra)[values(usoterra) == 73] = 71
usoterra[is.na(usoterra)] = -99

while (freq(usoterra,value = NA) != 0) {
  usoterra = focal(usoterra, matrix(1,3,3), fun = modal, na.rm = T, pad = T, NAonly = T)
}
#values(usoterra)[values(usoterra) == -99] = NA
#plot(usoterra)

#### Salvar

writeRaster(usoterra, file.path(dir2, "usoterra-PNCV-99.tif"), overwrite = T)

#### Reabrir o arquivo da pasta local

#usoterra = raster(paste(dir2, "usoterra-bacia-paranoa.tif", sep = "")); usoterra
#plot(usoterra)

############################################ SOLOS ############################################

#dir()
#dir(paste(dir, "Solos", sep = ""))
solos = read_sf(paste(dir, "Solos/mapa_solos_df_lat_long_wgs84.shp", sep = "")); solos

  # Novo shapefile a partir de um unico atributo
      
  #View(solos)
  #names(solos)
  solos = subset(solos, select = "COD_ORD2")
  solos = st_crop(solos, st_bbox(otto))
  solos = ms_dissolve(solos, "COD_ORD2")
  #plot(solos)

#### CRS

solos = st_transform(solos, 31983); solos

#### Vetor para raster

solos = as_Spatial(solos)
solos = rasterize(solos, usoterra, "COD_ORD2"); solos
#plot(solos)

#### Limites

solos = mask(solos, otto)
#plot(solos)
#solos.or = solos

#### Reclassificar valores das celulas (remover classe "outros" e "agua")

#freq(solos)
#zero = solos %in% 1
#plot(zero)
solos[is.na(solos)] = -99
values(solos)[values(solos) == 8] = 6
values(solos)[values(solos) == 11] = 12

while (freq(solos,value = NA) != 0) {
  solos = focal(solos, matrix(1,3,3), fun = modal, na.rm = T, pad = T, NAonly = T)
}
#values(solos)[values(solos) == -99] = NA
#plot(solos)

#### Salvar

writeRaster(solos, file.path(dir2, "solos-bacia-PNCV-99.tif"), overwrite = T)

#### Reabrir o arquivo da pasta local

#solos = raster(paste(dir2, "solos-bacia-PNCV.tif", sep = "")); solos
#plot(solos)

      #### Retirar classe agua
      
      # Para identificar as classes de solo presentes na area de estudo
      #solos = raster(paste(dir, "solos-PNCV-raster.tif", sep = ""))
      #plot(solos)
      #freq = freq(solos)
      #View(freq)
      
      #agua_sol = solos %in% 1
      #plot(agua_sol)
      #agua_sol[] = ifelse(agua_sol[] == 0,1,0)
      
      #crs(agua_sol) = crs(solos)
      #solos2 = solos * agua_sol
      #plot(solos2)
      #freq(solos2)
      #values(solos2)[values(solos2) == 0] = NA  # converte 0 em NA

      # Para salvar um raster apenas com a classe agua do mapa de solos
      #agua_sol2 = solos %in% 79233
      #agua_sol2[] = ifelse(agua_sol2[] == 1,1,NA)
      #plot(agua_sol2)
      #crs(agua_sol2) = crs(agua_sol)
      #writeRaster(agua_sol2, "agua-solos.tif")

#origin(solos) = origin(usoterra)

#solos[] = ifelse(solos[] == 1,1,0)
#usoterra[] = ifelse(usoterra[] == 33,1,0)
#plot(solos)
#plot(usoterra)
#solos[is.na(solos)] = 0
#usoterra[is.na(usoterra)] = 0

#agua = solos + usoterra
#plot(agua)
#values(agua)[values(agua) == 2] = 1
#agua[] = ifelse(agua[] == 1,1,NA)
#plot(agua)

#writeRaster(agua, file.path(dir, "agua-bacia-PNCV.tif"), overwrite = T)
#agua = raster("~/Documents/DadosEspaciais/agua-bacia-paranoa.tif")
plot(agua)

dir()

######################################## MALHA HIDRICA ########################################

#dir()
#dir(paste(dir, "hidro_trecho_drenagem", sep = ""))
dren = read_sf(paste(dir, "hidro_trecho_drenagem/hidro_trecho_drenagem.shp", sep = "")); dren

dren = st_crop(dren, st_bbox(otto.or))
#tm_shape(dren) + tm_fill(col = "regime") + tm_lines(alpha = 1)

#### CRS

dren = st_transform(dren, 31983); dren

#### Limites

dren = st_intersection(dren, st_make_valid(otto))
#tm_shape(dren) + tm_fill(col = "regime") + tm_lines(alpha = 1)

#### Salvar

dren = as_Spatial(dren)
writeOGR(dren, dsn = dir2, driver = "ESRI Shapefile", "dren-sub-PNCV", overwrite = T)

#### Reabrir o arquivo da pasta local

#dren = read_sf(paste(dir2, "dren-PNCV.shp", sep = "")); dren
#plot(dren)
#st_crs(dren) = st_crs("+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs")

      # Cruzar com agua do uso da terra e do solo?
      # Criar vetor com os represamentos?

###################################### FORMATACAO P/ WGEN ######################################

dirWGEN = "~/Documents/SWAT/EntradaTAB/WGEN-PNCV/"

#### Funcao para adicionar linhas

insert_line = function(file, text_to_insert, insert_at, write = FALSE){
  data = readLines(file)
  pre = data[1:insert_at-1]
  post = data[insert_at:length(data)]
  result = (c(pre, text_to_insert, post))
  if(write){
    writeLines(result, file)
  }
  return(result)
}

#### Loop para preencher todos os arquivos

# Criar uma nova pasta com arquivos sem a primeira linha

CFSR = dir(paste(dir2, "Climate/PCP/", sep = ""), pattern = "pcp0"); CFSR
i=1
for(i in 1:length(CFSR)) {
  obj = read.table(paste(dir2, "Climate/PCP/", CFSR[i], sep = ""), skip = 1)
  write.table(obj, paste("~/Documents/", CFSR[i], sep = ""), row.names = F, col.names = F, quote = F)
}

# Text
i=1
for(i in 1:length(CFSR)) {
  insert_line(paste("~/Documents/", CFSR[i], sep = ""), "Text", 1, write = T)
}

# Data inicial

i=1
for(i in 1:length(CFSR)) {
  insert_line(paste("~/Documents/", CFSR[i], sep = ""), "01/01/1980", 2, write = T)
}

# Data final
i=1
for(i in 1:length(CFSR)) {
  insert_line(paste("~/Documents/", CFSR[i], sep = ""), "12/31/2019", 3, write = T)
}

# .csv para .txt

dirPCP = "~/Documents/DataSetPNCV/Climate/PCP/"

csv = dir(dirPCP, pattern = "csv"); csv

i=1
for (i in 1:length(csv)) {
  arquivos = read.table(paste(dirPCP, csv[i], sep = ""))
  write.table(arquivos, paste(dirPCP, sub(".csv", "", csv[i]),".txt", sep = ""), row.names = F, col.names = F, quote = F, sep = ",")
}

# Arquivos em branco

# PNCV
CFSR = c("148-481","145-481","142-481","139-481","136-481","133-481","130-481","126-481","148-478","145-478","142-478","139-478","136-478","133-478","130-478","126-478","148-475","145-475","142-475","139-475","136-475","133-475","130-475","126-475","148-472","145-472","142-472","139-472","136-472","133-472","130-472","126-472","148-469","145-469","142-469","139-469","136-469","133-469","130-469","126-469","148-466","145-466","142-466","139-466","136-466","133-466","130-466","126-466","148-463","145-463","142-463","139-463","136-463","133-463","130-463","126-463")
ANA = c("pcp01346001","pcp01346002","pcp01346004","pcp01346005","pcp01347000","pcp01347001")
VarANA = c("_dwp", "_hhr", "_slr", "_tmp", "_wnd")
VarCFSR = c("_hhr", "_CFSR")
#length(CFSR)
#length(ANA)

i=1
for (i in 1:length(CFSR)) {
  for (j in 1:length(VarCFSR)) {
    write.table(x = NULL, paste(dirWGEN, CFSR[i], VarCFSR[j], ".txt", sep = ""), row.names = F, col.names = F)
  }
}

i=1
for (i in 1:length(ANA)) {
  for (j in 1:length(VarANA)) {
  write.table(x = NULL, paste(dir, ANA[i], VarANA[j], ".txt", sep = ""), row.names = F, col.names = F)
  }
}

#### Trocando -99.000 por -99.00 nos arquivos do clima

CFSR = dir(paste(dir2, "Climate/CFSR(1980-2014)/", sep = ""), pattern = "-"); CFSR
i=1
for(i in 1:length(CFSR)) {
  obj = read.table(paste(dir2, "Climate/CFSR(1980-2014)/", CFSR[i], sep = ""))
  obj[obj == "-99.000,-99.000" ] = "-99.00,-99.00"
  obj[obj == "-99.000" ] = "-99.00"
  write.table(obj, paste(dir2, "Climate/CFSR(1980-2014)2/", CFSR[i], sep = ""), row.names = F, col.names = F, quote = F)
}
