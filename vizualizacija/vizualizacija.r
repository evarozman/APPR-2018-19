# 3. faza: Vizualizacija podatkov

# grafi

### graf 



# zemljevid

source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")

library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)

zemljevid <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries", mapa = "zemljevidi", pot.zemljevida = "", encoding = "UTF-8") %>% 
  fortify()

zemljevid <- filter(zemljevid, CONTINENT == "Europe", long < 55 & long > -45 & lat > 30 & lat < 75)

ggplot() + geom_polygon(data=zemljevid, aes(x=long, y=lat, group=group, fill=id)) +
  guides(fill=FALSE) + labs(title="Evropa - osnovna slika")

