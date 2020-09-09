# 4. faza: Analiza podatkov

# združevanje v skupine

library(tmap)

zivlj_doba_razv <- data.frame(filter(zivlj_doba, SPOL=='m'))
zivlj_doba_razv <- zivlj_doba_razv[-c(3)]
prorac_razv <- filter(zdravstvo, LETO == '2016')
prorac_razv <- prorac_razv[c(1,3)]
postelje_razv <- filter(zdravstvo, LETO == '2016')
postelje_razv <- postelje_razv[c(1,4)]
tabela_razvrscanje1 <- inner_join(zivlj_doba_razv, prorac_razv, by="DRZAVA")
tabela_razvrscanje2 <- inner_join(tabela_razvrscanje1, postelje_razv, by="DRZAVA")
rownames(tabela_razvrscanje2) =  tabela_razvrscanje2$DRZAVA
tabela_razvrscanje <- tabela_razvrscanje2[-c(1)]

set.seed(1000)
clustri_zdr <- kmeans(scale(tabela_razvrscanje), 4, nstart=100)
tabela1 <- data.frame(DRZAVA = tabela_razvrscanje2$DRZAVA, ZIVLJENJSKA_DOBA = tabela_razvrscanje2$ZIVLJENJSKA_DOBA,
                      PRORACUN = tabela_razvrscanje2$PRORACUN, POSTELJE = tabela_razvrscanje2$POSTELJE, 
                      SKUPINA = factor(clustri_zdr$cluster, ordered = TRUE))
skupaj3 <- left_join(drzave, tabela1, by="DRZAVA")
zemljevid_cluster_zdr <- ggplot() + geom_polygon(data=left_join(zemljevid_evrope, skupaj3, by=c("NAME"="DRZAVA")), 
                                                aes(x=long, y=lat, group=group, fill=SKUPINA)) +
  geom_line() + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_manual(values = c('yellow', 'orange', 'orangered', 'red3'),
                    labels = c('1', '2', '3', '4'), na.value="grey") +
  labs(x = " ") +
  labs(y = " ") +
  ggtitle("Razvrstitev držav v skupine glede na zdravstveno stanje")

#print(zemljevid_cluster_zdr)


tabela_razvitost <- data.frame(filter(razvitost, LETO == '2016'))
tabela_razvitost$BREZPOSELNI <- -(tabela_razvitost$BREZPOSELNI)
rownames(tabela_razvitost) =  tabela_razvitost$DRZAVA
tabela_razvitost <- na.omit(tabela_razvitost)[-c(2)]
tabela_razvitost1 <- tabela_razvitost[-c(1)]

set.seed(1000)
clustri_razv <- kmeans(scale(tabela_razvitost1), 4, nstart=100)
tabela2 <- data.frame(DRZAVA = tabela_razvitost$DRZAVA, BDP = tabela_razvitost$BDP, 
                      DOHODEK = tabela_razvitost$DOHODEK, BREZPOSELNI = tabela_razvitost$BREZPOSELNI,
                      SKUPINA = factor(clustri_razv$cluster, ordered = TRUE))
skupaj4 <- left_join(drzave, tabela2, by="DRZAVA")
zemljevid_cluster_razv <- ggplot() + geom_polygon(data=left_join(zemljevid_evrope, skupaj4, by=c("NAME"="DRZAVA")), 
                                             aes(x=long, y=lat, group=group, fill=SKUPINA)) +
  geom_line() + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_manual(values = c('yellow', 'orange', 'orangered', 'red3'),
                    labels = c('1', '2', '3', '4'), na.value="grey") +
  labs(x = " ") +
  labs(y = " ") +
  ggtitle("Razvrstitev držav v skupine glede na razvitost")

#print(zemljevid_cluster_razv)

# regresija življenjske dobe

y <- group_by(zivlj_doba_regresija, LETO)
zivlj_doba_evropa <- summarise(y, POVPRECJE=mean(ZIVLJ_DOBA))

prileganje <- lm(data = zivlj_doba_evropa, POVPRECJE ~ LETO)

z <- data.frame(LETO=seq(2019, 2025, 1))
napoved <- mutate(z, POVPRECJE=predict(prileganje, z))

graf_regresija <- ggplot(zivlj_doba_evropa, aes(x=LETO, y=POVPRECJE)) +
  geom_smooth(method=lm, fullrange = TRUE, color = 'blue') +
  geom_point(data=napoved, aes(x=LETO, y=POVPRECJE), color='red', size=2) +
  geom_point() +
  labs(title='Napoved življenjske dobe za države Evropske unije', y="ŽIVLJENJSKA DOBA")

#print(graf_regresija)
