# setear librerías de trabajo
library(tidyverse); library(gganimate)

# importando dataset
dataset <- read.delim("dataset.csv", sep = ";")

# cambiando nombre a variables
colnames(dataset) <- c("year", "ctry", "ctry_code", "code_ano", "sch_enroll", 
                       "poverty_natlines", "poverty_5.50", "poverty_1.90", "lifexpect",
                       "housholddebt", "p90", "p99", "mobile_per_100", "inet_per_million")

#chequeamos que línea de la pobreza tiene más casos
length(na.omit(dataset$poverty_natlines))
length(na.omit(dataset$poverty_5.50)) 
length(na.omit(dataset$poverty_1.90))
# las últimas dos tienen la misma, asi que usaremos el umbra más bajo

dataset$sch_enroll

# primer plot animado
ggplot(dataset, aes(poverty_1.90, sch_enroll, size = lifexpect, colour = ctry)) +
  geom_jitter() +
  guides(colour = F) 
## desde acá agregar líneas de animación
  



