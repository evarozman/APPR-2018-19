# 3. faza: Vizualizacija podatkov

library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)

# grafi

### graf življenjske dobe v odvisnosti od števila zdravnikov

zivlj.doba <- zdravstveno_stanje[c("DRZAVA", "SPOL", "ZIVLJENJSKA_DOBA")]
st.osebja <- zdravstvo %>% filter(LETO==2014)
st.osebja <- st.osebja[c("DRZAVA", "ZDRAVNIKI")]
zdruzena <- inner_join(zivlj.doba, st.osebja, by=c("DRZAVA"))

graf_osebja <- ggplot(data=zdruzena, mapping=aes(x=ZDRAVNIKI, y=ZIVLJENJSKA_DOBA, color=SPOL)) +
  geom_point() + xlab("število zdravnikov") + ylab("življenjska doba")

print(graf_osebja)

### graf števila študentov v odvisnosti v državah z visokim in nizkim BDP-jem

graf_studentov <- 

print(graf_studentov)

### graf umrljivosti otrok v odvisnosti od proračuna

graf_umrljivosti

print(graf_umrljivosti)

### graf števila zdravih let v odvisnosti od brezposelnosti

graf_zdravih_let

print(graf_zdravih_let)

# zemljevid

source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")

zemljevid <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries", mapa = "zemljevidi", pot.zemljevida = "", encoding = "UTF-8") %>% 
  fortify()

zemljevid <- filter(zemljevid, CONTINENT == "Europe", long < 55 & long > -45 & lat > 30 & lat < 75)

ggplot() + geom_polygon(data=zemljevid, aes(x=long, y=lat, group=group, fill=id)) +
  guides(fill=FALSE) + labs(title="Evropa - osnovna slika")

