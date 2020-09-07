# 3. faza: Vizualizacija podatkov

library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(ggplot2)
library(munsell)


# grafi

### graf proračuna po državah Evropske unije za leto 2016

proracun_2016 <- proracun %>% filter(LETO==2016) %>% filter(PRORACUN!=0)

graf_proracuna_2016 <- ggplot(data=proracun_2016, aes(x=reorder(DRZAVA, PRORACUN), y=PRORACUN)) +
  geom_bar(stat="identity") + coord_flip() +
  labs(x="država", y="proračun (€/preb.)", title="Proračun po državah Evropske unije za leto 2016")

#print(graf_proracuna_2016)

### graf življenjske dobe v odvisnosti od števila zdravnikov

zivlj.doba <- zdravstveno_stanje[c("DRZAVA", "SPOL", "ZIVLJENJSKA_DOBA")]
st.zdravnikov <- zdravstvo %>% filter(LETO==2014)
st.zdravnikov <- st.zdravnikov[c("DRZAVA", "ZDRAVNIKI")]
skupaj.zdravniki <- inner_join(zivlj.doba, st.zdravnikov, by=c("DRZAVA"))

graf_osebja <- ggplot(data=skupaj.zdravniki, mapping=aes(x=ZDRAVNIKI, y=ZIVLJENJSKA_DOBA, colour=SPOL)) +
  geom_point() + labs(x="število zdravnikov (na 100.000 preb.)", y="pričakovana življenjska doba", 
                      title="Odvisnost življenjske dobe od spola in števila zdravnikov")

#print(graf_osebja)

### graf gibanja števila študentov v letih 2000-2017 v državah z največ in najmanj zdravniki

najvec.zdravnikov <- zdravstvo %>% filter(LETO==2016) %>% filter(STUDENTI_MEDICINE!=0) %>% top_n(3, ZDRAVNIKI)
najvec.zdravnikov <- c(najvec.zdravnikov$DRZAVA)

najmanj.zdravnikov <- zdravstvo %>% filter(LETO==2016) %>% filter(STUDENTI_MEDICINE!=0) %>% top_n(3, (-1)*ZDRAVNIKI)
najmanj.zdravnikov <- c(najmanj.zdravnikov$DRZAVA)

najvec.zdravnikov.tabela <- zdravstvo[c("DRZAVA", "LETO", "STUDENTI_MEDICINE")] %>% filter(DRZAVA %in% najvec.zdravnikov)
najmanj.zdravnikov.tabela <- zdravstvo[c("DRZAVA", "LETO", "STUDENTI_MEDICINE")] %>% filter(DRZAVA %in% najmanj.zdravnikov)

graf_studentov <- ggplot() +
  geom_line(data=najvec.zdravnikov.tabela, mapping=aes(group=DRZAVA, x=LETO, y=STUDENTI_MEDICINE), colour="red") +
  geom_line(data=najmanj.zdravnikov.tabela, mapping=aes(group=DRZAVA, x=LETO, y=STUDENTI_MEDICINE), colour="green") +
  labs(x="leto",y="število študentov medicine (na 100.000 preb.)", title="Število študentov medicine glede na \nštevilo zdravnikov v letih 2000-2017") + 
  legend("topright", legend=c("največ zdravnikov", "najmanj zdravnikov"),lty=1, cex=1, col=c("red", "green"))

#print(graf_studentov)

### graf umrljivosti otrok v odvisnosti od proračuna v letu 2014

umrljivost.otrok <- zdravstveno_stanje[c("DRZAVA", "SPOL","SMRT_OTROK")]
proracun.graf <- zdravstvo %>% filter(LETO==2014)
proracun.graf <- proracun.graf[c("DRZAVA", "PRORACUN")]

skupaj.umrljivost <- inner_join(umrljivost.otrok, proracun.graf, by="DRZAVA")

graf_umrljivosti <- ggplot(data=skupaj.umrljivost, mapping=aes(x=PRORACUN, y=SMRT_OTROK, colour=SPOL)) +
  geom_point() + labs(x="letni proračun (€/preb.)", y="število umrlih otrok do 1 leta (na 100.000 preb.)", 
                      title="Umrljivost otrok v letu 2014 glede na spol in proračun")

#print(graf_umrljivosti)

### gibanje proračuna za zdravstvo v obdobju 2010-2016 za države z najvišjim in najnižjim BDP-jem

skupaj <- inner_join(zdravstvo, razvitost, by=c("DRZAVA", "LETO"))

najvisji.bdp <- skupaj %>% filter(LETO==2016) %>% top_n(3, BDP)
najvisji.bdp <- c(najvisji.bdp$DRZAVA)

najnizji.bdp <- skupaj %>% filter(LETO==2016) %>% top_n(3, (-1)*BDP)
najnizji.bdp <- c(najnizji.bdp$DRZAVA)

najvisji.bdp.tabela <- zdravstvo[c("DRZAVA", "LETO", "PRORACUN")] %>% filter(DRZAVA %in% najvisji.bdp) %>% filter(LETO>=2010)
najnizji.bdp.tabela <- zdravstvo[c("DRZAVA", "LETO", "PRORACUN")] %>% filter(DRZAVA %in% najnizji.bdp) %>% filter(LETO>=2010)


graf_proracuna <- ggplot() +
  geom_line(data=najvisji.bdp.tabela, mapping=aes(group=DRZAVA, x=LETO, y=PRORACUN), col="blue") +
  geom_line(data=najnizji.bdp.tabela, mapping=aes(group=DRZAVA, x=LETO, y=PRORACUN), col="red") +
  labs(x="leto", y="letni proračun (€/preb.)", 
       title="Gibanje proračuna za zdravstvo v obdobju 2010-2016 \n za države z najvišjim in najnižjim BDP")
legend(2017,3000, legend=c("visok BDP", "nizek BDP"), lwd=1, cex=0.8, col=c("blue", "red"))

#print(graf_proracuna)


# zemljevid

source("lib/uvozi.zemljevid.r")

zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries", encoding="UTF-8") %>% fortify()

zemljevid_evrope <- filter(zemljevid, CONTINENT == "Europe", long < 55 & long > -45 & lat > 30 & lat < 75)

### zemljevid števila postelj v letu 2016

drzave <- unique(zemljevid_evrope$NAME) 
drzave <- as.data.frame(drzave, stringsAsFactors=FALSE) 
names(drzave) <- "DRZAVA"

postelje <- zdravstvo %>% filter(LETO==2016)
postelje <- postelje[c("DRZAVA", "POSTELJE")]

skupaj1 <- inner_join(drzave, postelje, by="DRZAVA")

zemljevid_postelj <- ggplot() + geom_polygon(data=inner_join(zemljevid_evrope, skupaj1, by=c("NAME"="DRZAVA")), 
                                                   aes(x=long, y=lat, group=group, fill=POSTELJE)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Število postelj")) +
  ggtitle("Število postelj na 100.000 prebivalcev v letu 2013") +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "white", high = "violetred",
                      space = "Lab", na.value = "#e0e0d1", guide = "black",
                      aesthetics = "fill")
print(zemljevid_postelj)

ziv_doba <- zdravstveno_stanje %>% filter(SPOL=="m")
ziv_doba <- ziv_doba[c("DRZAVA", "ZIVLJENJSKA_DOBA")]

skupaj2 <- inner_join(drzave, ziv_doba, by="DRZAVA")

zemljevid_zivlj_dobe <- ggplot() + geom_polygon(data=inner_join(zemljevid_evrope, skupaj2, by=c("NAME"="DRZAVA")), 
                                             aes(x=long, y=lat, group=group, fill=ZIVLJENJSKA_DOBA)) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Življenjska doba")) +
  ggtitle("Povprečna življenjska doba moških prebivalcev") +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "white", high = "violetred",
                      space = "Lab", na.value = "#e0e0d1", guide = "black",
                      aesthetics = "fill")
print(zemljevid_zivlj_dobe)
