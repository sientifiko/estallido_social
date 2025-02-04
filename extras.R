
# setear librer�as de trabajo
library(tidyverse); library(gganimate); library(transformr); 
library(imputeTS); library(gifski); library(patchwork); library(WDI); 
library(countrycode) ; library(vdem); library(survey); library(convey);
library(readxl)



# ========= EXTRA: VISUALIZANDO LA IMPUTACI�N DE DATOS =========

# visualizaremos el rendimiento en la interpolaci�n de una serie con
# distintos algoritmos

# importamos el �ndice de pobreza 1.90
pov190usd <- WDI(indicator = "SI.POV.DDAY", 
                 start = 1980, 
                 end = 2020)

# traemos la zona regional
pov190usd$region <- countrycode(pov190usd$iso2c,
                                origin = "iso2c",
                                destination = "region23")

# filtramos a latam por latam y cambiamos el nombre a la columna que usaremos
pov190usd <- pov190usd %>%
  filter(region %in% c("Central America","South America" ))
colnames(pov190usd)[3] <- "poverty.190"

# Quiero ver que casos tienen datos pre 90 para hacer una buena interpolaci�n.
# La interpolaci�n intenta inferir o construir la tendencia de datos que no tengo
# a partir de los que si, puedo construir hacia atr�s usando los datos posteriores, 
# pero es m�s riesgoso, y da menos confianza, a que si tengo a lo menos un punto pre 
# 90 que nos de m�s confianza en que la curva no se est� disparando a una nada 
# puramente te�rica
ctrys <- pov190usd %>% 
  filter(year %in% 1980:1990 & !is.na(poverty.190)) %>%
  select(country) %>% 
  unique()

# filtro por los pa�ses que si tienen datos hasta los 90
pov190usd2 <- pov190usd[pov190usd$country %in% ctrys$country,]

# reviso cual es el que tiene m�s datos perdidos, en este caso es guatemala
pov190usd2 %>% 
  group_by(country) %>%
  summarize(na= sum(is.na(poverty.190)))

# filtro por guatemala
gtm <- pov190usd2 %>% filter(country == "Guatemala")

# indico cuales son los a�os con datos perdidos
gtm$isna <- is.na(gtm$poverty.190)

# me da flojera editar esto, pero b�sicamente lo que hago es usar creo que todas
# las variantes de interpolaci�n que permite la librer�a imputeTS, sobre la serie de
# pobreza al 1.90 usd
gtm$lineal <- na.interpolation(gtm$poverty.190)
gtm$spline <- na.interpolation(gtm$poverty.190, option = "spline")
gtm$stine <- na.interpolation(gtm$poverty.190, option = "stine")
colnames(gtm)[3] <- "original"                                # ac� el cambi� el nombre a la variable
gtm$kalmanAA <- na.kalman(gtm$original, model = "auto.arima") # notar que empec� a usarlo desde ac�
gtm$kalmanST <- na.kalman(gtm$original, model = "StructTS")
gtm$m.average <- na.ma(gtm$original, weighting = "linear")
gtm$locfrev <- na_locf(gtm$original, option = "locf", na_remaining = "rev")

# un poco de brujer�a con dplyr, despivoteo la tabla para que quede todo el columnas
gtm2 <- gtm %>% gather("type", "value", c(3, 7:13))

# ordeno los factores de la interpolaci�n
gtm2$type <- factor(gtm2$type,
                    levels(as.factor(gtm2$type))[c(6,3,7,8,1,2,4,5)])

# y grafico, listo, pueden reconstruir esto para cualquier pa�s que quieran
# solo tienen que modificar los filtros m�s arriba
ggplot(gtm2, aes(year, value, color= isna, group=1)) +
  theme_classic() +
  geom_point() +
  facet_wrap(.~type, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="", y="% pobreza a 1.90 USD", 
       color="Es dato imputado?", 
       title = "Distintos algoritmos de imputaci�n",
       subtitle = "Guatemala")









