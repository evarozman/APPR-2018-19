# 4. faza: Analiza podatkov

zivlj_doba_razv <- data.frame(DRZAVA = zivlj_doba$DRZAVA[1:32], ZIVLJENJSKA_DOBA = 
                                ((zivlj_doba$ZIVLJENJSKA_DOBA[1:32] + zivlj_doba$ZIVLJENJSKA_DOBA[33:64])/2))

prorac_razv <- filter(zdravstvo, LETO == '2016')
prorac_razv <- prorac_razv[c(1,3)]

BDP_razv <- filter(razvitost, LETO == '2016')
BDP_razv <- BDP_razv[c(1,3)]

tabela_razvrscanje <- inner_join(zivlj_doba_razv, prorac_razv, BDP_razv, by="DRZAVA")
