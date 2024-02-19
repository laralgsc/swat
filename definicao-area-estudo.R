library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
library(reshape2)
library(stringr)

### OBJETOS IMPORTANTES

uf = c("BA", "MG")
dir = paste("/Users/laralanches/Documents/BolsaDTI/Gauges_BA_MG/", uf[2], "/", sep = "")
gauges <- str_remove(dir(dir, pattern = ".txt"), pattern = ".txt")
names <- paste("g", gauges, sep = "")

### IMPORTACAO E FORMATACAO DOS DADOS

i=1
objs <- list()
for (i in 1:length(gauges)) {
  data <- read_fwf(paste(dir, gauges[i], ".txt", sep = ""), col_positions = fwf_widths(c(6,6,6,9,7)), col_select = 1:4) %>% 
    mutate(date = make_date(year = X3, month = X2, day = X1)) %>%
    select(5, "flow" = X4)
  obj <- assign(paste("g", gauges[i], sep = ""), data)
  objs <- append(objs, list(obj))
}

remove(data, obj)

flow <- sapply(objs, '[[', 2) %>%
  as_tibble(.name_repair = ~names)
dateflow <- objs[[1]][1] %>%
  mutate(flow)

### AVALIACAO DOS DADOS

dataMelt <- melt(dateflow, "date")
ggplot(dataMelt, aes(x = date, y = value)) +
  geom_line() +
  xlim(as.Date("1900-01-01"),NA) +
  ylim(NA,1000) +
  facet_wrap(~ variable, ncol = 4)

#View(dataMelt %>% group_by(variable) %>% count(value == -1))

#estacoesBA <- c("g45810000", "g45840000")
#estacoesMG <- c("g54001000", "g54002990", "g54003000", "g54010000", "g54010005", "g54060000")

### AVALIACAO DAS ESTACOES ADEQUADAS

good <- dataMelt %>% 
  filter(variable == estacoesMG[c(5)])

good <- dataMelt %>% 
    filter(variable == "g45840000") %>% 
    group_by(variable, year = year(date), month = month(date)) %>% 
    summarise(value = mean(value))

# Single plot
ggplot(good, aes(x = date, y = value)) +
  geom_line() +
  xlim(as.Date("1950-01-01"),NA)

# Multiplot
ggplot(good, aes(x = date, y = value)) +
  geom_line() +
  xlim(as.Date("1900-01-01"),NA) +
  ylim(NA,1000) +
  facet_wrap(~ variable, ncol = 2)
  