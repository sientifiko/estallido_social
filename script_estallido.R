# setear librer�as de trabajo
library(tidyverse); library(gganimate); library(transformr); 
library(imputeTS); library(gifski); library(patchwork); library(WDI); 
library(countrycode) ; library(vdem); library(survey); library(convey);
library(readxl)


options(scipen = 999)


# ======= FUNCIONES A UTILIZAR EN EL SCRIPT ========
# Puedes colapsar esto si quieres ir directo al c�digo

# funci�n que imputa los datos de una variable seg�n determinada agregaci�n
interpolacion <- function(data, agregacion, variable){
  #' @param data un df
  #' @param agregacion el nivel de agregaci�n de la serie (eg, pais, regi�n, etc)
  #' @param variable la variable a ser imputada
  #' @return el mismo df, pero con la columna de valores imputaados
  

  require(tidyverse)
  agg <- data %>% select(!!agregacion) %>% unique()
  tls <- list()
  
  
  for(i in 1:nrow(agg)){
    
    tls[[i]] <- data[ data[[agregacion]] == agg[i,1], ][[variable]] %>%
      na_kalman(model = "StructTS")
    
  }
  
  dat <- do.call("rbind", tls) %>%
    t() %>%
    as.data.frame()
  
  dat <- dat %>%
    gather("", "value", 1:ncol(dat))
  
  data[[paste0("imp.", variable)]] <- dat[["value"]]
  
  return(data)
  
}# fin funci�n




# ====================== INTRODUCCI�N===============

# importando dataset del desarrollo humano
HDI <- read.csv("HDI.csv")
colnames(HDI)[4] <- "hdi"

HDI$continent <- countrycode(HDI$Code,
                             origin = "iso3c", 
                             destination = "continent")


# generando el promedio anual
yearhdi <- HDI %>%
  group_by(Year, continent) %>%
  summarize(meanhdi= mean(hdi))


# graficar el HDI
hdi_plot <- ggplot(yearhdi, aes(Year, meanhdi, color= reorder(continent, -meanhdi))) +
  theme_classic() +
  geom_line() +
  scale_x_continuous(breaks = seq(1870, 2015, 30)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", y= "�ndice de Desarrollo Humano", color ="")

# sacando datos del vdem
ed_vdem <- extract_vdem(name_pattern = "v2x_polyarchy",
                        include_external = F) 
ed_vdem <- select(ed_vdem, c(2, 4, 24, 31))

#agrupar por continente
ed_vdem <- ed_vdem %>%
  group_by(year, extended_continent) %>%
  summarize(meandemo = mean(v2x_polyarchy, na.rm = T))


# graficar el nivel de democracia
demo_plot <- ggplot(ed_vdem %>% na.omit(), aes(year, meandemo, 
                    color= reorder(extended_continent, -meandemo))) +
  theme_classic() +
  geom_line() +
  # scale_x_continuous(breaks = seq(1870, 2015, 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", y= "�ndice de Democracia Electoral", color ="")


hdi_plot + demo_plot + plot_layout(guides = "collect") +
  plot_annotation(title = "Un desarrollo complejo", 
                  theme = theme(plot.title = element_text(hjust = .5))) 


# ======================= CONSTRUCCI�N CURVAS DE LORENZ ANUALES =======================

descgtmar_casen_github(carpeta = "casenes")


ch_names <- function(data, vars){
  colnames(data) <- vars
  return(data)
}


c1990 <- read_rds("casenes/1990.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "r", "expr", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut")) %>%
  filter(edad>= 20 & edad <= 60)

c1992 <- read_rds("casenes/1992.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "r", "expr", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c1994 <- read_rds("casenes/1994.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "r", "expr", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c1996 <- read_rds("casenes/1996.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "r", "expr", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c1998 <- read_rds("casenes/1998.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "r", "expr", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c2000 <- read_rds("casenes/2000.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "r", "expr", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c2003 <- read_rds("casenes/2003.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "r", "expr", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c2006 <- read_rds("casenes/2006.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "r", "expr", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c2009 <- read_rds("casenes/2009.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "region", "expr", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c2011 <- read_rds("casenes/2011.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "region", "expr_full", "esc", "yautaj") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c2013 <- read_rds("casenes/2013.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "region", "expr", "esc", "yautcor") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c2015 <- read_rds("casenes/2015.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "region", "expr", "esc", "yaut") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)

c2017 <- read_rds("casenes/2017.rds") %>% 
  as.data.frame() %>%
  select("edad", "sexo", "region", "expr", "esc", "yaut") %>%
  ch_names(vars = c("edad", "sexo", "region", "expr", "esc", "yaut"))%>%
  filter(edad>= 20 & edad <= 60)



sv1990 <- svydesign(data = c1990, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv1992 <- svydesign(data = c1992, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv1994 <- svydesign(data = c1994, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv1996 <- svydesign(data = c1996, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv1998 <- svydesign(data = c1998, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv2000 <- svydesign(data = c2000, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv2003 <- svydesign(data = c2003, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv2006 <- svydesign(data = c2006, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv2009 <- svydesign(data = c2009, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv2011 <- svydesign(data = c2011, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv2013 <- svydesign(data = c2013, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv2015 <- svydesign(data = c2015, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)

sv2017 <- svydesign(data = c2017, 
                    ids = ~region,
                    nest = T,
                    weights = ~expr)


cp1990 <- convey_prep(sv1990)
cp1992 <- convey_prep(sv1992)
cp1994 <- convey_prep(sv1994)
cp1996 <- convey_prep(sv1996)
cp1998 <- convey_prep(sv1998)
cp2000 <- convey_prep(sv2000)
cp2003 <- convey_prep(sv2003)
cp2006 <- convey_prep(sv2006)
cp2009 <- convey_prep(sv2009)
cp2011 <- convey_prep(sv2011)
cp2013 <- convey_prep(sv2013)
cp2015 <- convey_prep(sv2015)
cp2017 <- convey_prep(sv2017)


lorenz1990 <- svylorenz(~yaut, cp1990, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz1992 <- svylorenz(~yaut, cp1992, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz1994 <- svylorenz(~yaut, cp1994, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz1996 <- svylorenz(~yaut, cp1996, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz1998 <- svylorenz(~yaut, cp1998, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz2000 <- svylorenz(~yaut, cp2000, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz2003 <- svylorenz(~yaut, cp2003, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz2006 <- svylorenz(~yaut, cp2006, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz2009 <- svylorenz(~yaut, cp2009, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz2011 <- svylorenz(~yaut, cp2011, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz2013 <- svylorenz(~yaut, cp2013, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz2015 <- svylorenz(~yaut, cp2015, quantiles = seq(0,1,.05), na.rm = TRUE)
lorenz2017 <- svylorenz(~yaut, cp2017, quantiles = seq(0,1,.05), na.rm = TRUE)


lista <- list(lorenz1990, 
              lorenz1992 ,
              lorenz1994 ,
              lorenz1996 ,
              lorenz1998 ,
              lorenz2000 ,
              lorenz2003 ,
              lorenz2006 ,
              lorenz2009 ,
              lorenz2011 ,
              lorenz2013 ,
              lorenz2015 ,
              lorenz2017) 


length(lista)
y <- c(seq(1990, 2000, 2), seq(2003, 2009, 3), seq(2011, 2017, 2))

length(y)



lorenz_hist <- function(list){
  
  ls <- list()
  y <- c(seq(1990, 2000, 2), seq(2003, 2009, 3), seq(2011, 2017, 2))
  
  for (i in 1:length(list)) {
    

    lorenz.data <- list[[i]][1] %>% 
      as.data.frame() %>%
      t() %>%
      as.data.frame()
    
    rownames(lorenz.data) <- NULL
    
    lorenz.data$quantiles <- seq(0,1,.05)
    
    lorenz.data$year <- y[i]
    
    colnames(lorenz.data)[1] <- "share"
    
    lorenz.data <- lorenz.data[, c(3, 2, 1)]

    ls[[i]] <- lorenz.data
  }

  return(do.call("rbind", ls) )
}


df_lorenz <- lorenz_hist(lista)


ggplot(df_lorenz %>% filter(year %in% c(1990, 2000, 2009, 2017)),
       aes(quantiles, share, group= year, color=as.factor(reorder(year, -year)))) +
  theme_classic() +
  geom_line() +
  geom_line(aes(y= quantiles), color = "black") +
  # scale_color_gradient(low= "black", 
  #                       high= "green", 
  #                       limits= c(1990, 2017)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.caption.position = "plot") +
  labs(x= "% poblaci�n", y= "% ingreso", color = "A�o", 
       title = "Evoluci�n distribuci�n ingreso Chile 1990 - 2017",
       subtitle = "Ingreso aut�nomo poblaci�n 20 a 60 a�os. Se usaron expansores regionales",
       caption = "Elaboraci�n propia con base CASEN 1990 a 2017")

df_lorenz$diff_share <- 1 - df_lorenz$share

#================== CONSTRUCCI�N DE GR�FICAS DE DEMOCRACIA EN LATAM =====


# sacando datos del vdem
ed_vdem <- extract_vdem(name_pattern = "v2x_polyarchy",
                        include_external = F) 
ed_vdem <- select(ed_vdem, c(1, 2, 4, 24, 30))


polity <- read_xls("p5v2018.xls") %>% select(c(4:6, 11, 12))
polity$region <- countrycode(polity$scode,
                             origin = "p4c", 
                             destination = "region23")
polity$iso3  <- countrycode(polity$scode,
                            origin = "p4c", 
                            destination = "iso3c")

polity <- polity %>%
  filter(region %in% c("Central America","South America" ))


# filtrando por latam
ed_vdem <- ed_vdem %>% 
  filter(extended_region %in% c("Central America","South America" ))

vplot <- ggplot(ed_vdem %>% filter(year >= 1960), 
       aes(year,v2x_polyarchy, 
           color =reorder(vdem_country_text_id, -v2x_polyarchy))) +
  theme_classic() +
  geom_line() +
  facet_wrap(.~extended_region, nrow = 2) +
  geom_vline(xintercept = 1990) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x="", y= "",
       subtitle = "�ndice de Democracia Electoral (0 a 1)", 
       color="")


p.plot <- ggplot(polity %>% filter(year >= 1960), 
       aes(year,polity2, 
           color =reorder(iso3, -polity2))) +
  theme_classic() +
  geom_line() +
  facet_wrap(.~region, nrow = 2) +
  geom_vline(xintercept = 1990) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none") +
  labs(x="", y= "", 
       subtitle = "�ndice Democracia Polity IV (-10 a 10)",
       color="")

vplot + p.plot + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "La ola democr�tica en Latam",
                  caption = "Elaboraci�n propia con base a V-dem y Polity IV") &
  theme(legend.position = "none")


# ============== CONSTRUCCI�N DE GR�FICAS DE DESARROLLO EN LATAM ======



pov320usd <- WDI(indicator = "SI.POV.LMIC", 
                 start = 1980, 
                 end = 2020)

pov320usd$region <- countrycode(pov320usd$iso2c,
                                origin = "iso2c",
                                destination = "region23")

pov320usd <- pov320usd %>%
  filter(region %in% c("Central America","South America" ))
colnames(pov320usd)[3] <- "poverty.320"


ctrys <- pov320usd %>% 
  filter(year %in% 1980:1990 & !is.na(poverty.320)) %>%
  select(country) %>% 
  unique()


pov320usd2 <- pov320usd[pov320usd$country %in% ctrys$country,]

pov320usd2 <- interpolacion(pov320usd2, "country", "poverty.320")


grp.pov <- pov320usd2 %>%
  filter(year %in% c(1990, 2020)) %>%
    select(country, year, imp.poverty.320)


pov.plot <- ggplot(grp.pov, aes(reorder(country, 
                            imp.poverty.320), 
                    imp.poverty.320, 
                    group= country)) +
  theme_minimal() +
  geom_line(color = "black") +
  geom_point(aes( shape= as.factor(year),
                  color= as.factor(year)), size=3) +
  coord_flip() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(color="", shape= "", 
       x="", y ="% personas viviendo con menos \nde 3.20 USD al d�a")
  

dif.pov <- grp.pov %>%
  spread(2, 3)

dif.pov$dif <- dif.pov$`2020` -  dif.pov$`1990`

pov.plot2 <- ggplot(dif.pov, aes(reorder(country, -dif), dif)) +
  theme_classic() +
  geom_bar(stat = "identity", 
           fill=ifelse(dif.pov$dif>0, "darkred", "darkgreen")) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x="", y= "Variaci�n pobreza �ndice 3.20 USD, \nentre 1990 y 2020")


pov.plot + pov.plot2 +
  plot_annotation(title = "La reducci�n de la pobreza en Latam",
                  theme = theme(plot.title = element_text(hjust = .5)),
                  caption = "PPC al 2011, elaboraci�n propia con base a \ndatos del Banco Mundial")




pov320usd %>% 
  filter(country %in% c("Argentina", "Chile"))


ggplot(pov320usd2 %>% 
         filter(country %in% c("Argentina", "Chile")),
       aes(year, imp.poverty.320,
           shape=country,
           color=is.na(poverty.320))) +
  theme_classic() +
  geom_point() +
  geom_line(aes(group=country)) +
  labs(x="", y="%",
       shape="", color="Es dato imputado?",
       title = "Porcentaje personas que viven con 3.20 USD o menos al d�a")







