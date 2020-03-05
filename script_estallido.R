# setear librerías de trabajo
library(tidyverse); library(gganimate); library(transformr); library(imputeTS)

# importando dataset
dataset <- read.delim("dataset.csv", sep = ";")

# cambiando nombre a variables
colnames(dataset) <- c("year", "ctry", "ctry_code", "code_ano", "sch_enroll", 
                       "poverty_natlines", "poverty_5.50", "poverty_1.90", "lifexpect",
                       "housholddebt", "p90", "p99", "mobile_per_100", "inet_per_million", "idh",
                       "egaldem", "protest_google_search")

#chequeamos que línea de la pobreza tiene más casos
length(na.omit(dataset$poverty_natlines))
length(na.omit(dataset$poverty_5.50)) 
length(na.omit(dataset$poverty_1.90))
# las últimas dos tienen la misma, asi que usaremos el umbra más bajo

# usar un dataset más pequeño para ploteo
data2 <- dataset[dataset$year >= 1980, c(1,3,5,8,9)]

# imputar datos perdidos
paises <- unique(data2$ctry_code)
for(i in paises){
  # la función na_interpolate requiere como mínimo 2 valores no nulos
  # imputamos pobreza
  if(sum(!is.na(data2$poverty_1.90[data2$ctry_code==i]))>2){
    data2$poverty_1.90[data2$ctry_code==i] <- data2$poverty_1.90[data2$ctry_code==i] %>% 
      na_interpolation()
  } else {
    data2 <- data2[!data2$ctry_code==i,]
  }
  # imputamos matrícula escolar
  if(sum(!is.na(data2$sch_enroll[data2$ctry_code==i]))>2){
    data2$sch_enroll[data2$ctry_code==i] <- data2$sch_enroll[data2$ctry_code==i] %>% 
      na_interpolation()
  } else {
    data2 <- data2[!data2$ctry_code==i,]
  }
  #imputamos esperanza de vida
  if(sum(!is.na(data2$lifexpect[data2$ctry_code==i]))>2){
    data2$lifexpect[data2$ctry_code==i] <- data2$lifexpect[data2$ctry_code==i] %>% 
      na_interpolation()
  } else {
    data2 <- data2[!data2$ctry_code==i,]
  }
}
rm(i, paises)

# primer plot animado
desa_humano <- ggplot(data2, aes(poverty_1.90, sch_enroll, 
                                   size = lifexpect, 
                                   label = ctry_code,
                                   colour = ifelse(ctry_code=="CHL", "red", "black"))) +
  theme_bw() +
  geom_text() +
  guides(colour = F) +
  theme(text = element_text(size = 25)) +
  labs(title = "Año: {frame_time}", 
       x = "% personas viviendo con menos de 1.90 USD al día",
       y = "% bruto matrícula educación primaria",
       size = "Esperanza \nde vida") +
  transition_time(year)

animate(desa_humano,  duration = 30, width = 900)
# se pueden alterar dimensiones de gráfica con width = x, height = y
rm(desa_humano)

# plotear el IDH
idh <- dataset[, c(1,2,15)]
idh <- na.omit(idh)

# manipular el dataset
idh2 <- idh %>% filter(year == 1970 | year == 2015)

# plot de la IDH en américa latina
ggplot(idh2, aes(idh, reorder(ctry, -idh))) +
  theme_bw() +
  theme(text = element_text(size = 20), legend.position = "top",
        axis.text.y = element_text(size = 10)) +
  geom_line(aes(group = ctry, color = ifelse(ctry == "Chile", "red", "black"))) +
  geom_point(aes(color = as.factor(year)), size = 3) +
  # geom_text(aes(label = as.factor(year), color = as.factor(year)), size = 3) +
  scale_x_continuous(limits = c(0,1)) +
  scale_color_discrete(breaks = c(1970, 2015)) +
  labs(x = "Índice de Desarrollo Humano",
       y = "", color = "")


# recortar dataset 
data3 <- dataset[,c(1,3,15,16)]

# imputar 
paises <- unique(data3$ctry_code)
for(i in paises){
  # la función na_interpolate requiere como mínimo 2 valores no nulos
  # imputamos pobreza
  if(sum(!is.na(data3$idh[data3$ctry_code==i]))>2){
    data3$idh[data3$ctry_code==i] <- data3$idh[data3$ctry_code==i] %>% 
      na_interpolation()
  } else {
    data3 <- data3[!data3$ctry_code==i,]
  }
  # imputamos matrícula escolar
  if(sum(!is.na(data3$egaldem[data3$ctry_code==i]))>2){
    data3$egaldem[data3$ctry_code==i] <- data3$egaldem[data3$ctry_code==i] %>% 
      na_interpolation()
  } else {
    data3 <- data3[!data3$ctry_code==i,]
  }
}
rm(i, paises)

# generar plot de relación IDH vs democracia igualitaria
idh_egaldem <- ggplot(data3, aes(idh, egaldem, 
                  colour = ifelse(ctry_code=="CHL", "red", "black"))) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 25),
        plot.title = element_text(hjust = .5, size = 40, face = "bold")) +
  geom_text(aes(label = ctry_code, fontface = 2, size = 20) ) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Año: {frame_time}",
       x = "IDH",
     y = "Índice democracia igualitaria") +
  transition_time(year)
  
# crear gif
animate(idh_egaldem,  duration = 30, width = 900)

# jugando con datos de google trends
data4 <- dataset %>% filter(ctry_code == "CHL" & year >= 2000) %>% 
  select(1, 15:17)

# imputando
data4$idh <- data4$idh %>% na_interpolation()
data4$egaldem <- data4$egaldem %>% na_interpolation()
data4$protest_google_search <- data4$protest_google_search %>% na_interpolation()

# plot
ggplot(data4, aes(idh, egaldem, alpha = protest_google_search)) +
  theme_bw()+
  geom_smooth(show.legend = F, color = "red") +
  geom_text(aes(label = year, fontface = 2), angle = 90, size = 10,
          position=position_jitter(width=.007,height=.007)) +
  theme(legend.position = "top", text = element_text(size = 20)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "IDH", y = "Índice de Democracia igualitaria", 
       alpha = 'Búsquedas de "Protesta" \nen Google, Chile')




