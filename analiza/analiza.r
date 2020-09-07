# 4. faza: Analiza podatkov

# združevanje v skupine

library(tmap)

zivlj_doba_razv <- data.frame(DRZAVA = zivlj_doba$DRZAVA[1:32], ZIVLJENJSKA_DOBA = 
                                ((zivlj_doba$ZIVLJENJSKA_DOBA[1:32] + zivlj_doba$ZIVLJENJSKA_DOBA[33:64])/2))
prorac_razv <- filter(zdravstvo, LETO == '2016')
prorac_razv <- prorac_razv[c(1,3)]
postelje_razv <- filter(zdravstvo, LETO == '2016')
postelje_razv <- postelje_razv[c(1,4)]
BDP_razv <- filter(razvitost, LETO == '2016')
BDP_razv <- BDP_razv[c(1,3)]
tabela_razvrscanje1 <- inner_join(zivlj_doba_razv, prorac_razv, by="DRZAVA")
tabela_razvrscanje2 <- inner_join(tabela_razvrscanje1, postelje_razv, by="DRZAVA")
tabela_razvrscanje3 <- inner_join(tabela_razvrscanje2, BDP_razv, by="DRZAVA")
rownames(tabela_razvrscanje3) =  tabela_razvrscanje3$DRZAVA
tabela_razvrscanje <- tabela_razvrscanje3[-c(1)]

clustri <- kmeans(scale(tabela_razvrscanje), 4)
tabela1 <- data.frame(DRZAVA = tabela_razvrscanje3$DRZAVA, DRZAVA = tabela_razvrscanje3$DRZAVA, 
                      PRORACUN = tabela_razvrscanje3$PRORACUN, POSTELJE = tabela_razvrscanje3$POSTELJE, 
                      BDP = tabela_razvrscanje3$BDP, SKUPINA = factor(clustri$cluster, ordered = TRUE))
skupaj3 <- inner_join(drzave, tabela1, by="DRZAVA")
zemljevid_cluster <- ggplot() + geom_polygon(data=inner_join(zemljevid_evrope, skupaj3, by=c("NAME"="DRZAVA")), 
                                                aes(x=long, y=lat, group=group, fill=SKUPINA)) +
  geom_line() + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_manual(values = c('yellow', 'orange', 'orangered', 'red3'),
                    labels = c('1', '2', '3', '4')) +
  labs(x = " ") +
  labs(y = " ") +
  ggtitle("Razvrstitev držav po po skupinah")

#print(zemljevid_cluster)

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