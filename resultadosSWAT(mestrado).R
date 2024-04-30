dir = "~/Documents/Projeto/ResultadosBACIA/"
setwd(dir)
dir()

library(raster)
library(rgdal)
library(sf)
library(rmapshaper)
library(stars)
library(tmap)
library(fs)
library(fasterize)
library(rgeos)
library(proj4)
library(ggplot2)

######################################## Mapas ########################################

HRU = read_sf(paste(dir, "HRUs-BACIA-Val.shp", sep = ""))
id = "-BACIA-Val.tif"

UC = read_sf("~/Documents/DadosEspaciais/Limites/UCs-mest.shp")
UC = st_transform(UC, 31983); UC

aguaBACIA = raster("~/Documents/DadosEspaciais/agua-BACIA.tif"); aguaBACIA

aguaBACIA0 = aguaBACIA
aguaBACIA0[] = ifelse(aguaBACIA0[] == 1,0,1)
aguaBACIA0[is.na(aguaBACIA0)] = 1
#plot(aguaBACIA0)

# Salvar mapas dos componentes hidrologicos

H2O = c("PRECIPmm","ETmm","PERCmm","GW_RCHGmm","DA_RCHGmm","SA_STmm","DA_STmm","SURQ_GENmm","SURQ_CNTmm","LATQ_mm","GW_Qmm","WYLD_Qmm")

for(i in 1:length(H2O)) {
        obj = fasterize(HRU, aguaBACIA, field = H2O[i])
        writeRaster(obj, file.path(paste(dir, H2O[i], id, sep = "" )), overwrite = T)
}

# Salvar mapas da porcentagem no balanco hidrico

precip = raster(paste(dir,"PRECIPmm", id, sep =""))

for(i in 1:length(H2O)) {
        raster = raster(paste(dir, H2O[i], id, sep = "" ))
        prct = raster/precip
        writeRaster(prct, file.path(paste(dir, H2O[i], "-prct", id, sep = "" )), overwrite = T)
}

#### PRECIPITACAO ####

PRECIP = raster(paste(dir,"PRECIPmm", id, sep =""))
#plot(PRECIP)

#### SUBSURFACE FLOW ####

LAT = raster(paste(dir,"LATQ_mm", id, sep =""))
BAS = raster(paste(dir,"GW_Qmm", id, sep =""))
SS = LAT + BAS

SSp = SS/PRECIP

writeRaster(SS, file.path(paste(dir, "SSmm", id, sep = "" )), overwrite = T)
writeRaster(SSp, file.path(paste(dir, "SSmm-prct", id, sep = "" )), overwrite = T)

#### Componentes hidrologicos ####

H2Oi = c("PRECIPmm","ETmm","PERCmm","GW_RCHGmm","DA_RCHGmm","SA_STmm","DA_STmm","SURQ_GENmm","SURQ_CNTmm","LATQ_mm","GW_Qmm","SSmm","WYLD_Qmm")
H2Oii = c("PRECIPmm-prct","ETmm-prct","PERCmm-prct","GW_RCHGmm-prct","DA_RCHGmm-prct","SA_STmm-prct","DA_STmm-prct","SURQ_GENmm-prct","SURQ_CNTmm-prct","LATQ_mm-prct","GW_Qmm-prct","SSmm-prct","WYLD_Qmm-prct")

#1 = PRECIP
#2 = ET
#3 = PERC
#4 = GW_RCHG
#5 = DA_RCHG
#6 = SA_ST
#7 = DA_ST
#8 = SURQ_GEN
#9 = SURQ_CNT
#10 = LATQ
#11 = GW_Q
#12 = SS
#13 = WYLD_Q

CMPuc = mask(CMP, UC)
#plot(CMPuc, main = H2Oi[2])

CMPout = CMP
mascara = raster(paste(dir,"mascaraBACIA.tif", sep =""))
values(CMPout)[values(CMPout) == 0] = -99
CMPout = CMPout*mascara
values(CMPout)[values(CMPout) == 0] = NA
values(CMPout)[values(CMPout) == -99] = 0
#plot(CMPout, main = H2Oi[2])

IN = values(CMPuc)
OUT = values(CMPout)
TOT = values(CMP)
data_list = list(IN, OUT, TOT)
x = mean(IN, na.rm = T); x
y = mean(OUT, na.rm = T); y
z = mean(TOT, na.rm = T); z

plot(CMP)

# Boxplot total

b = boxplot(data_list, names = c("PA","NP","Total"), col = "transparent", ylim = c(0,1500), yaxs = "i", boxwex = 0.5, yaxt = "n", xaxt = "n", outline = F, frame.plot=F)
axis(2, at = seq(0, 1500, by = 100), tck=-0.04, lwd = 2, labels = F)
axis(2, at = seq(0, 1500, by = 300), line = 0.7, lwd = 0, cex.axis = 2.4)
mtext(1, at = seq_along(b$names), text = b$names, 1, cex = 2.4)
points(x = seq_along(b$names), y = c(x,y,z))

# Boxplot fracao

b = boxplot(data_list, names = c("PA","NP","Total"), col = "transparent", ylim = c(0,1), yaxs = "i", boxwex = 0.5, yaxt = "n", xaxt = "n", outline = F, frame.plot=F)
axis(2, at = seq(0, 1, by = 0.1), tck=-0.04, lwd = 2, labels = F)
axis(2, at = seq(0, 1, by = 0.2), line = 0.7, lwd = 0, cex.axis = 2.4)
mtext(1, at = seq_along(b$names), text = b$names, 1, cex = 2.4)
points(x = seq_along(b$names), y = c(x,y,z))

pdf(paste(H2Oii[12], id, sep = ""), width=6, height=8)
fig.myplot()
dev.off()

#### MODIS ####

#MODIS_BACIA = raster("~/Documents/DadosEspaciais/ET-MODIS/valBACIA.tif")
#MODIS_BACIA = projectRaster(MODIS_BACIA, crs = 31983, method = "ngb", res = 500)
#MODIS_BACIA = crop(MODIS_BACIA, PRECIP)
#baciaBACIA = read_sf("~/Documents/DadosEspaciais/Limites/bacia-BACIA.shp")
#MODIS_BACIA = mask(MODIS_BACIA, baciaBACIA)
#plot(MODIS_BACIA)
#MODIS_BACIA = projectRaster(MODIS_BACIA, crs = 31983, method = "ngb", res = 12.5)
#values(MODIS_BACIA)[values(MODIS_BACIA) > 6000] = NA
#MODIS_BACIA = projectRaster(MODIS_BACIA, PRECIP)
#writeRaster(MODIS_BACIA, file.path(dir, "MODIS-BACIA-orig.tif"), overwrite = T)

ETm = raster("~/Documents/Projeto/ResultadosSWAT/MODIS-BACIA.tif")
#plot(ETm)

ETs = raster(paste(dir,"ETmm", id, sep =""))
#values(ETs)[values(ETs) == 0] = -99
#ETs = ETs*aguaBACIA0
#values(ETs)[values(ETs) == 0] = NA
#values(ETs)[values(ETs) == -99] = 0
#masc = projectRaster(ETm, ETs)
#ETs = mask(ETs, masc)
#plot(ETs)

SWAT = values(ETs)
MODIS = values(ETm)
data_list = list(SWAT, MODIS)
S = mean(SWAT, na.rm = T); S
M = mean(MODIS, na.rm = T); M

# Boxplot total

fig.myplot <- function() {
        b = boxplot(data_list, main = "Paranã River sub-basin", names = c("SWAT","MODIS"), col = "transparent", ylim = c(0,1500), yaxs = "i", xaxs = "i", boxwex = 0.5, yaxt = "n", xaxt = "n", outline = F, frame.plot=F)
        axis(2, at = seq(0, 1500, by = 100), lwd = 2, labels = F)
        axis(2, at = seq(0, 1500, by = 300), line = 0, lwd = 0, cex.axis = 1.5)
        mtext(1, at = seq_along(b$names), text = b$names, 1, cex = 1.5)
        #mtext(2, text = "ET (mm/year)", 3, cex = 1.5)
        points(x = seq_along(b$names), y = c(S,M))
        box(bty = "L", lwd = 2)
}

pdf("SWATxMODIS-BACIA.pdf", width=4, height=6)
fig.myplot()
dev.off()

######################################## Summary Statistics ########################################

#### Boxplots ####

aguaBACIA = raster("~/Documents/DadosEspaciais/agua-bacia-BACIA.tif"); aguaBACIA

aguaBACIA0 = aguaBACIA
aguaBACIA0[] = ifelse(aguaBACIA0[] == 1,0,1)
aguaBACIA0[is.na(aguaBACIA0)] = 1
#plot(aguaBACIA0)

UC = read_sf("~/Documents/DadosEspaciais/Limites/UCs-mest.shp")
UC = st_transform(UC, 31983); UC

mascara = raster(paste(dir,"mascaraBACIA.tif", sep =""))

H2Om = c("ETmm","WYLD_Qmm","SURQ_GENmm","LATQ_mm","GW_Qmm")
fig.myplot <- function() {
        b = boxplot(data_list, names = c("PA","NP","Total"), col = "transparent", ylim = c(0,1500), yaxs = "i", boxwex = 0.5, yaxt = "n", xaxt = "n", outline = F, frame.plot=F)
        points(x = seq_along(b$names), y = c(x,y,z))
        axis(2, at = seq(0, 1500, by = 100), tck=-0.04, lwd = 2, labels = F)
        axis(2, at = seq(0, 1500, by = 300), line = 0.7, lwd = 0, cex.axis = 2.4)
        mtext(1, at = seq_along(b$names), text = b$names, 1, cex = 2.4)
}
for(i in 1:length(H2Om)) {
        raster = raster(paste(dir, H2Om[i], id, sep = ""))
        values(raster)[values(raster) == 0] = -99
        raster = raster*aguaBACIA0
        values(raster)[values(raster) == 0] = NA
        values(raster)[values(raster) == -99] = 0
        rasteruc = mask(raster, UC)
        rasterout = raster
        values(rasterout)[values(rasterout) == 0] = -99
        rasterout = rasterout*mascara
        values(rasterout)[values(rasterout) == 0] = NA
        values(rasterout)[values(rasterout) == -99] = 0
        IN = values(rasteruc)
        OUT = values(rasterout)
        TOT = values(raster)
        data_list = list(IN, OUT, TOT)
        x = mean(IN, na.rm = T)
        y = mean(OUT, na.rm = T)
        z = mean(TOT, na.rm = T)
        pdf(paste(H2Om[i], id, sep = ""), width=6, height=8)
        fig.myplot()
        dev.off()
}

H2Op = c("ETmm-prct","SURQ_GENmm-prct","PERCmm-prct","SSmm-prct")
fig.myplot <- function() {
        b = boxplot(data_list, names = c("PA","NP","Total"), col = "transparent", ylim = c(0,1), yaxs = "i", boxwex = 0.5, yaxt = "n", xaxt = "n", outline = F, frame.plot=F)
        points(x = seq_along(b$names), y = c(x,y,z))
        axis(2, at = seq(0, 1, by = 0.1), tck=-0.04, lwd = 2, labels = F)
        axis(2, at = seq(0, 1, by = 0.2), line = 0.7, lwd = 0, cex.axis = 2.4)
        mtext(1, at = seq_along(b$names), text = b$names, 1, cex = 2.4)
}
for(i in 1:length(H2Op)) {
        raster = raster(paste(dir, H2Op[i], id, sep = ""))
        values(raster)[values(raster) == 0] = -99
        raster = raster*aguaBACIA0
        values(raster)[values(raster) == 0] = NA
        values(raster)[values(raster) == -99] = 0
        rasteruc = mask(raster, UC)
        rasterout = raster
        values(rasterout)[values(rasterout) == 0] = -99
        rasterout = rasterout*mascara
        values(rasterout)[values(rasterout) == 0] = NA
        values(rasterout)[values(rasterout) == -99] = 0
        IN = values(rasteruc)
        OUT = values(rasterout)
        TOT = values(raster)
        data_list = list(IN, OUT, TOT)
        x = mean(IN, na.rm = T)
        y = mean(OUT, na.rm = T)
        z = mean(TOT, na.rm = T)
        pdf(paste(H2Op[i], id, sep = ""), width=6, height=8)
        fig.myplot()
        dev.off()
}

#### Codigo individual ####

IN = values(CMPuc)
OUT = values(CMPout)
TOT = values(CMP)
data_list = list(IN, OUT, TOT)
x = mean(IN, na.rm = T)
y = mean(OUT, na.rm = T)
z = mean(TOT, na.rm = T)

# Boxplot total

b = boxplot(data_list, names = c("PA","NP","Total"), col = "transparent", ylim = c(0,1500), yaxs = "i", boxwex = 0.5, yaxt = "n", xaxt = "n", outline = F, frame.plot=F)
axis(2, at = seq(0, 1500, by = 100), tck=-0.04, lwd = 2, labels = F)
axis(2, at = seq(0, 1500, by = 300), line = 0.7, lwd = 0, cex.axis = 2.4)
mtext(1, at = seq_along(b$names), text = b$names, 1, cex = 2.4)
points(x = seq_along(b$names), y = c(x,y,z))

# Boxplot fracao

b = boxplot(data_list, names = c("PA","NP","Total"), col = "transparent", ylim = c(0,1), yaxs = "i", boxwex = 0.5, yaxt = "n", xaxt = "n", outline = F, frame.plot=F)
axis(2, at = seq(0, 1, by = 0.1), tck=-0.04, lwd = 2, labels = F)
axis(2, at = seq(0, 1, by = 0.2), line = 0.7, lwd = 0, cex.axis = 2.4)
mtext(1, at = seq_along(b$names), text = b$names, 1, cex = 2.4)
points(x = seq_along(b$names), y = c(x,y,z))

# Area (km2)

areaTOTAL = (ncell(ET) - freq(ET, value = NA))*(12.5^2)/10^6; areaTOTAL
areaUC = (ncell(ETuc) - freq(ETuc, value = NA))*(12.5^2)/10^6; areaUC
areaOUT = (ncell(ETout) - freq(ETout, value = NA))*(12.5^2)/10^6; areaOUT

# ET

quantile(ETuc)
quantile(ETout)
cellStats(ETuc, mean)
cellStats(ETout, mean)
cellStats(ETuc, sd)
cellStats(ETout, sd)
cellStats(ETuc, sum)
cellStats(ETout, sum)
cellStats(ETuc, 'rms')

cellStats(ETuc, sum)/areaUC
cellStats(ETout, sum)/areaOUT

# Percolacao

quantile(PERCuc)
quantile(PERCout)
cellStats(PERCuc, mean)
cellStats(PERCout, mean)
cellStats(PERCuc, sd)
cellStats(PERCout, sd)
cellStats(PERCuc, sum)
cellStats(PERCout, sum)
cellStats(PERCuc, 'rms')

cellStats(PERCuc, sum)/areaUC
cellStats(PERCout, sum)/areaOUT

# Variancia
# Diferenca significativa
# Analise de componentes principais


#### Analises ####

#HRU = read_sf(paste(dir, "HRUs-BACIA-Val.shp", sep = ""))
#names(HRU)
var = HRU$GW_Qmm
aov = aov(var ~ HRU$LANDUSE + HRU$SOIL + HRU$SLOPE_BAND); summary (aov)

b = boxplot(var ~ HRU$LANDUSE, cex.axis = 0.5, outline = F)
a = aggregate(var, list(HRU$LANDUSE), mean); a
points(x = seq_along(a[,1]), y = a[,2])

b = boxplot(var ~ HRU$SOIL, cex.axis = 0.5, outline = F)
a = aggregate(var, list(HRU$SOIL), mean); a
points(x = seq_along(a[,1]), y = a[,2])

b = boxplot(var ~ HRU$SLOPE_BAND, cex.axis = 0.5, outline = F)
a = aggregate(var, list(HRU$SLOPE_BAND), mean); a
points(x = seq_along(a[,1]), y = a[,2])

