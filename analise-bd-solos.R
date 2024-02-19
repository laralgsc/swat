library(readxl)
library(tidyverse)
library(mgsub)
library(gghighlight)
library(patchwork)
library(sf)

## Functions
filter.soils <- function(data, cod, partial.match = F, not_cod = NA, preview = T) {
  if(partial.match == F & is.na(not_cod)) {
    # Filtra pela celula completa
    filtered <- data %>%
      select(1,2,4,7,8,10,11,12,15,17,31,41,44,45,75:79,106,111,112,115:121,124:126,130,136,149,162,164,173) %>% 
      filter(Codigo_Solo == cod) %>% 
      arrange(Codigo_PA, profundi_1) %>% 
      group_by(Codigo_PA)
  }
  if(partial.match == T & is.na(not_cod)) {
    # Filtra por parte de texto nas celulas
    filtered <- data %>%
      select(1,2,4,7,8,10,11,12,15,17,31,41,44,45,75:79,106,111,112,115:121,124:126,130,136,149,162,164,173) %>% 
      filter(grepl(cod, Codigo_Solo)) %>% 
      arrange(Codigo_PA, profundi_1) %>% 
      group_by(Codigo_PA)
  }
  if(partial.match == T & !is.na(not_cod)) {
    # Filtra por parte de texto nas celulas
    filtered <- data %>%
      select(1,2,4,7,8,10,11,12,15,17,31,41,44,45,75:79,106,111,112,115:121,124:126,130,136,149,162,164,173) %>% 
      filter(grepl(cod, Codigo_Solo)) %>% 
      filter(!grepl(not_cod, Codigo_Solo)) %>% 
      arrange(Codigo_PA, profundi_1) %>% 
      group_by(Codigo_PA)
  }
  if(preview == T) {
    print(unique(filtered$Codigo_Solo))
    return(filtered)
  }
  if(preview == F) {
    return(filtered)
  }
}
standardize.horizon <- function(data, precision = c("master", "transition"), str_rem) {
  if (precision == "master") {
    return <- 
    str_remove_all(data$simbolo_ho, str_rem) %>% 
      str_extract("[A-Z]") %>% 
      mgsub(c("H", "D"), c("O", "R")) %>% 
      table() # Extrai somente a PRIMEIRA letra maiuscula
  }
  if (precision == "transition") {
    return <- 
    str_remove_all(data$simbolo_ho, str_rem) %>% 
      str_extract("[A-Z]+") %>% 
      {gsub("H", "O", .)} %>% 
      {gsub("D", "R", .)} %>% 
      table() # Extrai TODAS as letras maiusculas consecutivas (e necessario remover alguns caracteres que ficam entre elas)
              # Codigo menos eficiente que o anterior mas traz uma alternativa para usar gsub com pipe
  }
  return(return)
}
graph <- function(data, horiz, y, fill1, fill2, ylimits, breaks, uf, BR = F, basin, bbox.dist = 0) {
  latmin <- st_bbox(basin)[2] - bbox.dist
  latmax <- st_bbox(basin)[4] + bbox.dist
  longmin <- st_bbox(basin)[1] - bbox.dist
  longmax <- st_bbox(basin)[3] + bbox.dist
  
  dataBR <- data %>% 
    filter(ho_simplif == horiz) %>% 
    arrange(.data[[fill1]])
  
  dataUF <- data %>% 
    filter(ho_simplif == horiz, UF == uf) %>% 
    arrange(.data[[fill2]])

  assign("dataBR", dataBR, envir = .GlobalEnv)
  assign("dataUF", dataUF, envir = .GlobalEnv)
  
  barBR <- dataBR %>% 
    ggplot(aes_string("Codigo_PA", y, fill = fill1)) +
    geom_col(position = "dodge2", color = "black", size = 1) +
    scale_y_continuous(limits = ylimits, breaks = breaks) +
    scale_x_discrete(limits = dataBR$Codigo_PA, labels = NULL) +
    gghighlight(between(gcs_latitu, latmin, latmax), 
                between(gcs_longit, longmin, longmax), 
                keep_scales = T, unhighlighted_params = list(fill = NULL))

  barUF <- dataUF %>% 
    ggplot(aes_string("Codigo_PA", y, fill = fill2)) +
    geom_col(position = "dodge2", color = "black", size = 1) +
    scale_y_continuous(limits = ylimits, breaks = breaks) +
    scale_x_discrete(limits = dataUF$Codigo_PA, labels = NULL) +
    gghighlight(between(gcs_latitu, latmin, latmax), 
                between(gcs_longit, longmin, longmax), 
                keep_scales = T, unhighlighted_params = list(fill = NULL))

  boxBR <- dataBR %>% 
    ggplot(aes_string(fill1, y, fill = fill1)) +
    geom_boxplot(position = "dodge2", color = "black", size = 1) +
    scale_y_continuous(limits = ylimits, breaks = breaks) +
    gghighlight(between(gcs_latitu, latmin, latmax), 
                between(gcs_longit, longmin, longmax), 
                keep_scales = T, unhighlighted_params = list(fill = NULL))

  boxUF <- dataUF %>% 
    ggplot(aes_string(fill2, y, fill = fill2)) +
    geom_boxplot(position = "dodge2", color = "black", size = 1) +
    scale_y_continuous(limits = ylimits, breaks = breaks) +
    gghighlight(between(gcs_latitu, latmin, latmax), 
                between(gcs_longit, longmin, longmax), 
                keep_scales = T, unhighlighted_params = list(fill = NULL))

  if(!all(is.na(dataUF[y])) & BR == F) {
    print(summary(dataUF[y]))
    return(barUF + boxUF + plot_layout(nrow = 2) + plot_annotation(title = paste("Horizonte", horiz)))
  }
  if(!all(is.na(dataUF[y])) & BR == T) {
    print(summary(dataBR[y]))
    print(summary(dataUF[y]))
    return(barBR + barUF + boxBR + boxUF + plot_layout(nrow = 2) + plot_annotation(title = paste("Horizonte", horiz), tag_levels = list(c("BR", uf))))
  }
  if(all(is.na(dataUF[y]))) {
    warning(paste("No", uf, "data"))
    print(summary(dataBR[y]))
    return(barBR + boxBR + plot_layout(nrow = 2) + plot_annotation(title = paste("Horizonte", horiz)))
  }
}

## Variables
PA <- read_xlsx("/Users/laralagesanches/Documents/BolsaDTI/pontos-amostragem.xlsx")
dados <- read_xlsx("/Users/laralagesanches/Documents/BolsaDTI/base-dados-solos.xlsx", na = "NULL")

## Pre processing
joined <- full_join(PA, dados, by = c("Codigo_PA" = "codigo_pon"), suffix = c(".PA", ".dados"))

## Noteworthy
# Total de 885 pontos de amostragem sem dados quantitativos, dos quais 505 tambem nao contam com classificacao do solo, verificado em:
# filter(joined, is.na(sigla)) %>% View()

# Horizontes: H/O (depende se umido ou seco), A, E, B, C, R/D.
# Numa classificação completa outros símbolos são utilizados para agregar mais informações, tais como:
# Algarismos arábicos – associados às letras maiúsculas referem-se a subdivisão dos horizontes.
# Algarismos romanos – precedendo as letras maiúsculas, são empregados para designar descontinuidades litológicas tanto em horizontes como em camadas (exemplo: II B2, II, C1, III C2 etc.).
# Letras minúsculas – associadas às letras maiúsculas, indicam características observadas naquele horizonte, tais como acumulação de argila por iluviação (t), forte cimentação (m), distúrbios pela aração ou outros fatores (p) etc.

##################################### CONSTRUCAO DA TABELA #####################################

## Variables
str_rem <- "/|I|V|X|CAMADA|TRADAGEM|TRADAG|TRADA|TRAD|T|Linha C.|Leito|Camada"
  # A ordem importanta porque, por exemplo, se T estivesse antes de TRAD, um valor TRAD viraria RAD e nao NA.

filtered <- filter.soils(data = joined,
                         cod = "RQ",
                         partial.match = T,
                         not_cod = NA,
                         preview = T) # Checar se as classes filtradas representam apenas o grupo de interesse, senao usar not_cod

      ## Mid checking
      table(count(filtered)[,2]) # Frequencia do numero de horizontes nos perfis
      summary(count(filtered)[,2]) # Estatisticas do numero de horizontes nos perfis
      unique(filtered$simbolo_ho) # Cada nome de horizonte no conjunto de dados
      
      standardize.horizon(data = filtered, precision = "transition", str_rem)
      standardize.horizon(data = filtered, precision = "master", str_rem)
        #ATENCAO a possiveis simbolos extras que precisam ser retirados para se identificar o horizonte corretamente

simplified <- 
  mutate(filtered, 
         ho_simplif = mgsub(str_extract(str_remove_all(simbolo_ho, str_rem), "[A-Z]"),
                            c("H", "D"), c("O", "R")),
         .after = simbolo_ho) %>% 
  group_by(Codigo_PA, ho_simplif) %>% 
  summarise(across(c(1:10,20,21), first), max_profund = max(profundida), min_profund = min(profundi_1), across(26:36, mean))
# Adiciona coluna com codigo padronizado e simplicado dos horizontes, depois
# agrupa por ponto de amostragem E horizonte para calcular a MEDIA dos horizontes com multiplas subcamadas

      ## Mid checking
      #simplified <- mutate(simplified, ho_simplif = gsub("B", "C", ho_simplif))
        #Opcional em alguns solos muito rasos em que classificacoes diferentes podem se referir ao mesmo horizonte
      table(simplified$ho_simplif) # Frequencia dos horizontes apos o agrupamento das subcamadas
      View(simplified)

####################################### ANALISE DOS DADOS #######################################

## Constants
param <- c("max_profund", "densidade_", "carbono_or",
           "argila", "silte", "terra_fina",
           "condutivid", "equivalent", "ph_h2o") # condutivid = SOL_EC; equivalent = SOL_CAL

## Variables
basin <- st_read("/Users/laralagesanches/Documents/BolsaDTI/farinha-basin.gpkg")
uf <- "MA"
cod.nivel3 <- "RQo"
horiz <- "C"
y <- param[9]

graph(data = simplified, #filter(simplified, max_profund < 9000),
      horiz = horiz,
      y = y,
      fill1 = "UF",
      fill2 = "Codigo_Solo",
      ylimits = c(0, NA),
      breaks = seq(0, 1000, by = 100),
      uf = uf,
      BR = F,
      basin = basin,
      bbox.dist = 0.5)

# Informacoes sobre a estrutura radicular nos perfis
simplified %>%
  filter(grepl("raíz", Informacoes_Complementares...10)) %>% View()

# Estatisticas dos perfis no Brasil, na UF e na vizinhanca da bacia
summary(dataBR[y])
summary(dataUF[y])
summary(filter(dataUF, Codigo_Solo == cod.nivel3)[y]) # filtrado pela classificacao do solo em terceiro nivel
dataUF %>% 
  filter(between(gcs_latitu, st_bbox(basin)[2]*1.1, st_bbox(basin)[4]*1.1), between(gcs_longit, st_bbox(basin)[1]*1.1, st_bbox(basin)[3]*1.1)) %>% 
  {summary(.[y])}

# Perfis identificados na vizinhanca da bacia
simplified %>% 
  filter(between(gcs_latitu, st_bbox(basin)[2]*2000, st_bbox(basin)[4]*2000), between(gcs_longit, st_bbox(basin)[1]*1.1, st_bbox(basin)[3]*1.1),
         ho_simplif == "A") %>% View ()

