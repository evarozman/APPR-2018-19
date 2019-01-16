# 3. faza: Vizualizacija podatkov

library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(ggplot2)
library(munsell)


# grafi

### graf življenjske dobe v odvisnosti od števila zdravnikov

zivlj.doba <- zdravstveno_stanje[c("DRZAVA", "SPOL", "ZIVLJENJSKA_DOBA")]
st.zdravnikov <- zdravstvo %>% filter(LETO==2014)
st.zdravnikov <- st.zdravnikov[c("DRZAVA", "ZDRAVNIKI")]
skupaj.zdravniki <- inner_join(zivlj.doba, st.zdravnikov, by=c("DRZAVA"))

graf_osebja <- ggplot(data=skupaj.zdravniki, mapping=aes(x=ZDRAVNIKI, y=ZIVLJENJSKA_DOBA, color=SPOL)) +
  geom_point() + xlab("število zdravnikov (na 100.000 preb.)") + ylab("pričakovana življenjska doba")

print(graf_osebja)

### graf gibanja števila študentov v letih 2000-2017

graf_studentov <- ggplot(data=zdravstvo, mapping=aes(x=LETO, y=STUDENTI_MEDICINE)) +
  geom_line() + xlab("leto") + ylab("število študentov medicine (na 100.000 preb.)")

print(graf_studentov)

### graf umrljivosti otrok v odvisnosti od proračuna v letu 2017

umrljivost.otrok <- zdravstveno_stanje[c("DRZAVA", "SPOL","SMRT_OTROK")]
proracun.graf <- zdravstvo %>% filter(LETO==2014)
proracun.graf <- proracun.graf[c("DRZAVA", "PRORACUN")]

skupaj.umrljivost <- inner_join(umrljivost.otrok, proracun.graf, by="DRZAVA")

graf_umrljivosti <- ggplot(data=skupaj.umrljivost, mapping=aes(x=PRORACUN, y=SMRT_OTROK, color=SPOL)) +
  geom_point() + xlab("letni proračun (€/preb.)") + ylab("število umrlih otrok do 1 leta (na 100.000 preb.)")

print(graf_umrljivosti)

### gibanje proračuna za zdravstvo v obdobju 2000-2016 za države z najvišjim in najnižjim BDP-jem

skupaj <- inner_join(zdravstvo, razvitost, by=c("DRZAVA", "LETO"))

najvisji.bdp <- skupaj %>% filter(LETO==2016) %>% top_n(3, BDP)
najvisji.bdp <- c(najvisji.bdp$DRZAVA)
najvisji.bdp.tabela <- zdravstvo[c("DRZAVA", "LETO", "PRORACUN")] %>% filter(DRZAVA %in% najvisji.bdp)

najnizji.bdp <- skupaj %>% filter(LETO==2016) %>% top_n(3, (-1)*BDP)
najnizji.bdp <- c(najnizji.bdp$DRZAVA)
najnizji.bdp.tabela <- zdravstvo[c("DRZAVA", "LETO", "PRORACUN")] %>% filter(DRZAVA %in% najnizji.bdp)

graf_proracuna <- ggplot() +
  geom_line(data=najvisji.bdp.tabela, mapping=aes(x=LETO, y=PRORACUN, color="red")) +
  geom_line(data=najnizji.bdp.tabela, mapping=aes(x=LETO, y=PRORACUN, color="green")) +
  xlab("leto") + ylab("letni proračun (€/preb.)")

print(graf_proracuna)


# zemljevid

source("lib/uvozi.zemljevid.r")

zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries") %>% fortify()

zemljevid <- filter(zemljevid, CONTINENT == "Europe", long < 55 & long > -45 & lat > 30 & lat < 75)

ggplot() + geom_polygon(data=zemljevid, aes(x=long, y=lat, group=group, fill=id)) +
  guides(fill=FALSE) + labs(title="Evropa - osnovna slika")

