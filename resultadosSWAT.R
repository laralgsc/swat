library(sf)
library(terra)
library(tidyverse)
library(fasterize)

past <- function(...) {
  paste(..., sep = "")
} # Modifica o default de paste para sep = ""

### Variaveis
dir <- "/Users/laralagesanches/Documents/BolsaDTI/resultados-alto-jequitin"; setwd(dir); dir()
lulc_filepath <- "/Users/laralagesanches/Documents/BolsaDTI/LULC_ofc.tif"
uc_filepath <- "/Users/laralagesanches/Documents/BolsaDTI/resultados-alto-jequitin/unc_21_br_cem_v2_alto-jequitin.gpkg"
hru_filepath <- "/Users/laralagesanches/Documents/BolsaDTI/resultados-alto-jequitin/hru-alto-jequitin-cal.gpkg"
file_id <- "_cal.tif"
EPSG <- 31983

### Pre-processing
agua_NA <- ifel(rast(lulc_filepath) == 33, NA, 1)
#plot(agua_NA)

UC <- st_read(uc_filepath) %>% 
  st_transform(EPSG)
#plot(UC[11])

HRU <- st_read(hru_filepath)

H2O <- HRU %>% st_drop_geometry() %>% select(ends_with("mm")) %>% names()
H2Oi <- append(H2O, "SSmm", after = which(H2O == "GW_Qmm"))
H2Oii <- past(H2Oi[-1], "_pct")
data_descrip <- list(abs = H2Oi, pct = H2Oii)

### Functions
# Le os valores dos rasters (PA, NP e Total), atribui a uma tibble e compila uma lista com todas as tibbles de todos os componentes
data.all <- function(H2O_vector, file_id, agua_NA, mask, datatype = c("abs", "pct"), dir = NULL){
  past <- function(...) {
    paste(..., sep = "")
  }
  assign("data_all", list())
  for (i in 1:length(H2O_vector)) {
    CMP <- rast(past(H2O_vector[i], file_id))*agua_NA
    CMPuc <- mask(CMP, mask)
    CMPout <- mask(CMP, mask, inverse = T)
    
    data <- tibble(PA = values(CMPuc, mat = F), NP = values(CMPout, mat = F), Total = values(CMP, mat = F))
    data <- filter(data, rowSums(is.na(data)) != ncol(data)) #remove linhas com NA em todas as colunas
    
    data_all <- append(data_all, setNames(list(data), H2O_vector[i]))
  }
  assign(past("data_all_", datatype), data_all, envir = .GlobalEnv)
}
# Constroi um boxplot baseado no tipo de valor do componente (absoluto ou porcentagem)
boxplot.base <- function(data, datatype = c("abs", "pct"), main = NULL, ylim_abs = NULL) {
  if(datatype == "abs"){
    a <- unique(tibble(boxplot(data, plot = F)$group,
                       boxplot(data, plot = F)$out))
    boxplot(data, col = c("#B9CEAC", "#f4eeb1", "#bc9e82"), main = main, ylim = ylim_abs, xaxt = "n", yaxt = "n", yaxs = "i", frame.plot = F, outline = F)
    mtext(1, at = seq(length(data)), text = names(data), line = .5)
    axis(2, at = seq(ylim_abs[1], ylim_abs[2], by = 100), las = 1, tck = -0.02)
    points(x = seq(length(data)), y = colMeans(data, na.rm = T), pch = 9)
    points(x = a[[1]], y = a[[2]])
  }
  if (datatype == "pct"){
    a <- unique(tibble(boxplot(data, plot = F)$group,
                       boxplot(data, plot = F)$out))
    boxplot(data, col = c("#B9CEAC", "#f4eeb1", "#bc9e82"), main = main, ylim = c(0,1), xaxt = "n", yaxt = "n", yaxs = "i", frame.plot = F, outline = F)
    mtext(1, at = seq(length(data)), text = names(data), line = .5)
    axis(2, at = seq(0, 1, by = 0.1), las = 1, tck = -0.02)
    points(x = seq(length(data)), y = colMeans(data, na.rm = T), pch = 9)
    points(x = a[[1]], y = a[[2]])
  }
}
violin.ggplot <- function(data, datatype = c("abs", "pct"), x, y, title, ylim_abs) {
  if(datatype == "abs"){
    ggplot(data, aes(.data[[x]], .data[[y]], fill = .data[[x]], color = .data[[x]])) +
      geom_violin(trim = F, scale = "count", show.legend = F, width = 1.2, adjust = 2) +
          scale_fill_manual(values = c("#B9CEAC", "#f4eeb1", "#bc9e82")) +
          scale_color_manual(values = c("#B9CEAC", "#f4eeb1", "#bc9e82")) +
          geom_boxplot(width = 0.05, outlier.shape = NA, fill = "transparent", color = "black") +
          stat_summary(fun = "mean", geom = "point", shape = 20, size = 1, color = "black", show.legend = F) +
          ggtitle(title) +
          ylab("Annual mean (mm)") +
          scale_y_continuous(limits = ylim_abs, breaks = seq(ylim_abs[1], ylim_abs[2], by = 100), expand = c(0, 0)) +
          theme(text = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.ticks.length.y = unit(2, units = "mm"),
                axis.line.y = element_line(colour = "black", linewidth = 0.5),
                panel.background = element_blank())
  }
  if(datatype == "pct"){
    ggplot(data, aes(.data[[x]], .data[[y]], fill = .data[[x]], color = .data[[x]])) +
      geom_violin(trim = F, scale = "count", show.legend = F, width = 1.2, adjust = 2) +
      scale_fill_manual(values = c("#B9CEAC", "#f4eeb1", "#bc9e82")) +
      scale_color_manual(values = c("#B9CEAC", "#f4eeb1", "#bc9e82")) +
      geom_boxplot(width = 0.05, outlier.shape = NA, fill = "transparent", color = "black") +
      stat_summary(fun = "mean", geom = "point", shape = 20, size = 1, color = "black", show.legend = F) +
      ggtitle(title) +
      ylab("Fraction of precipitation") +
      scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
      theme(text = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.length.y = unit(2, units = "mm"),
            axis.line.y = element_line(colour = "black", linewidth = 0.5),
            panel.background = element_blank())
  }
}

######################################## Mapas ########################################

## Salvar mapas dos componentes hidrologicos
raster <- raster(agua_NA) #necessario por uma questao de compatibilidade com fasterize
for(i in 1:length(H2O)) {
  fasterize(HRU, raster, H2O[i]) %>% 
    writeRaster(past(H2O[i], file_id), overwrite = T)
}
rm(raster)

# Subsurface flow
(rast(past("LATQ_mm", file_id)) + rast(past("GW_Qmm", file_id))) %>% 
  writeRaster(past("SSmm", file_id), overwrite = T)

## Salvar mapas da porcentagem dos componentes hidrologicos no balanco hidrico
precip <- rast(past("PRECIPmm", file_id))
H2Ominus <- H2O[-1]
for(i in 1:length(H2Ominus)) {
  (rast(past(H2Ominus[i], file_id))/precip) %>% 
    writeRaster(past(H2Ominus[i], "_pct", file_id), overwrite = T)
}
rm(H2Ominus)

# Subsurface flow
(rast(past("SSmm", file_id))/precip) %>% 
  writeRaster(past("SSmm", "_pct", file_id), overwrite = T)

###################################### Boxplots ######################################

# Para analisar apenas uma parte dos componentes, melhor refazer o objeto data_descrip com os componentes desejados
# Para checar a posicao de cada elemento: tibble(seq(length(H2Oi)), H2Oi); tibble(seq(length(H2Oii)), H2Oii)
# data_descrip <- list(abs = H2Oi[], pct = H2Oii[])

## Compilar em uma lista as tabelas com os valores de cada componente
for (i in c(1,2)) {
  data.all(H2O_vector = data_descrip[[i]],
           file_id = file_id,
           agua_NA = agua_NA,
           mask = UC,
           datatype = names(data_descrip[i]))
}

# Criar uma lista com as duas listas (abs + pct): data_all <- list(abs = data_all_abs, pct = data_all_pct)

## Salvar boxplot de cada componente em pdf
for (i in 1:length(data_descrip)) {
  for(j in 1:length(data_descrip[[i]])) {
    pdf(past(data_descrip[[i]][j], sub(".tif", ".pdf", file_id)), width = 6, height = 8, useDingbats = T)
    boxplot.base(data = get(past("data_all_", names(data_descrip[i])))[[j]],
                 datatype = names(data_descrip[i]),
                 main = past(names(get(past("data_all_", names(data_descrip[i])))[j]),
                           str_remove(file_id, ".tif")),
                 ylim_abs = c(0,1000))
    dev.off()
  }
}

## Salvar violin plot de cada componente em pdf
for (i in 1:length(data_descrip)) {
  for(j in 1:length(data_descrip[[i]])) {
    pdf(past(data_descrip[[i]][j], sub(".tif", "_violin.pdf", file_id)), width = 6, height = 8)
    violin.ggplot(data = na.omit(stack(get(past("data_all_", names(data_descrip[i])))[[j]])),
                   datatype = names(data_descrip[i]),
                   x = "ind",
                   y = "values",
                   title = past(names(get(past("data_all_", names(data_descrip[i])))[j]),
                                str_remove(file_id, ".tif")),
                   ylim_abs = c(0,1000))
    dev.off()
  }
}

# Media de cada coluna: data_all_abs %>% sapply(colMeans, na.rm = T)

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

#### Codigo individual ####

# Para checar a posicao de cada elemento: for (n in c("H2Oi", "H2Oii")) {tibble(seq(length(get(n))), get(n)) %>% print()}
# Valor maximo de cada tabela: data_all_abs %>% sapply(max, na.rm = T)
data <- data_all_abs$GW_Qmm
datatype <- "abs"
data <- data_all_pct$`ETmm-pct`
datatype <- "pct"

#pdf(past(names(data_all_abs)[1], sub(".tif", ".pdf", file_id)), width = 6, height = 8, useDingbats = T)
boxplot.base(data = data,
             datatype = datatype,
             main = past(names(data_all_abs)[11],
                         str_remove(file_id, ".tif")),
             ylim_abs = c(0,1000))
#dev.off()

#tictoc::tic()
#pdf(past(names(data_all_abs)[2], sub(".tif", "violin.pdf", file_id)), width = 6, height = 8)
violin.ggplot(data = na.omit(stack(data)),
              datatype = "abs",
              x = "ind",
              y = "values",
              title = names(data_all_abs)[2],
              ylim_abs = c(0,1000))
#dev.off()
#tictoc::toc()

colMeans(data, na.rm = T)

#ggplot(stack(data), aes(ind, values)) +
#  stat_boxplot(geom = "errorbar", width = 0.5) +
#  geom_boxplot(fill = c("#B9CEAC", "#f4eeb1", "#bc9e82")) +
#  stat_summary(fun = "mean", geom = "point", shape = 9, size = 4) +
#  ggtitle(H2Oi[i]) +
#  ylab("Annual mean (mm)") +
#  scale_y_continuous(limits = c(0,800), breaks = seq(0, 800, by = 100), expand = c(0, 0)) +
#  theme(text = element_text(size = 12),
#        axis.title.x = element_blank(),
#        axis.ticks.x = element_blank(),
#        axis.ticks.length.y = unit(2, units = "mm"),
#        axis.line.y = element_line(colour = "black", linewidth = 0.5),
#        panel.background = element_blank())
#
# (nao usei pois o processamento foi MUITO lento)

#### Analises estatisticas ####

var <- HRU$GW_Qmm
aov <- aov(var ~ HRU$LANDUSE + HRU$SOIL + HRU$SLOPE_BAND); summary (aov)

b <- boxplot(var ~ HRU$LANDUSE, cex.axis = 0.5, outline = F)
a <- aggregate(var, list(HRU$LANDUSE), mean); a
points(x = seq_along(a[,1]), y = a[,2])

b <- boxplot(var ~ HRU$SOIL, cex.axis = 0.5, outline = F)
a <- aggregate(var, list(HRU$SOIL), mean); a
points(x = seq_along(a[,1]), y = a[,2])

b <- boxplot(var ~ HRU$SLOPE_BAND, cex.axis = 0.5, outline = F)
a <- aggregate(var, list(HRU$SLOPE_BAND), mean); a
points(x = seq_along(a[,1]), y = a[,2])

# Variancia?
# Diferenca significativa?
# Analise de componentes principais?

#### Extra ####

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