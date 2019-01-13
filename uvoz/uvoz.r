library(readr)
library(reshape2)
library(dplyr)



### 1. TABELA

zivljenska_doba <- read_csv("podatki/zivlj_doba.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
zivljenska_doba <- zivljenska_doba[, -c(3, 5)]

moski_zdrava_leta <- filter(zivljenska_doba, INDIC_HE == "Healthy life years in absolute value at birth - males")
moski_zdrava_leta["SPOL"] <- "m"
moski_zdrava_leta <- moski_zdrava_leta[-c(2), -c(1)]
names(moski_zdrava_leta) <- (c("DRZAVA", "ZDRAVA LETA", "SPOL"))
moski_zdrava_leta$DRZAVA <- gsub("^Germany.*", "Germany", moski_zdrava_leta$DRZAVA)

zenske_zdrava_leta <- filter(zivljenska_doba, INDIC_HE == "Healthy life years in absolute value at birth - females")
zenske_zdrava_leta["SPOL"] <- "f"
zenske_zdrava_leta <- zenske_zdrava_leta[-c(2), -c(1)]
names(zenske_zdrava_leta) <- (c("DRZAVA", "ZDRAVA LETA", "SPOL"))
zenske_zdrava_leta$DRZAVA <- gsub("^Germany.*", "Germany", zenske_zdrava_leta$DRZAVA)

moski_zivlj_doba <- filter(zivljenska_doba, INDIC_HE == "Life expectancy in absolute value at birth - males")
moski_zivlj_doba["SPOL"] <- "m"
moski_zivlj_doba <- moski_zivlj_doba[-c(2), -c(1)]
names(moski_zivlj_doba) <- (c("DRZAVA", "ZIVLJENSKA DOBA", "SPOL"))
moski_zivlj_doba$DRZAVA <- gsub("^Germany.*", "Germany", moski_zivlj_doba$DRZAVA)

zenske_zivlj_doba <- filter(zivljenska_doba, INDIC_HE == "Life expectancy in absolute value at birth - females")
zenske_zivlj_doba["SPOL"] <- "f"
zenske_zivlj_doba <- zenske_zivlj_doba[-c(2), -c(1)]
names(zenske_zivlj_doba) <- (c("DRZAVA", "ZIVLJENSKA DOBA", "SPOL"))
zenske_zivlj_doba$DRZAVA <- gsub("^Germany.*", "Germany", zenske_zivlj_doba$DRZAVA)

zdrava_leta <- rbind(moski_zdrava_leta, zenske_zdrava_leta)
zivlj_doba <- rbind(moski_zivlj_doba, zenske_zivlj_doba)

zdravstveno_stanje <- inner_join(zdrava_leta, zivlj_doba, by=c("DRZAVA", "SPOL"))

st_prebivalcev <- read_csv("podatki/st_prebiv.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_prebivalcev <- st_prebivalcev[-c(2), -c(1, 3, 5)]
names(st_prebivalcev) <- (c("DRZAVA", "STEVILO PREBIVALCEV"))
st_prebivalcev$DRZAVA <- gsub("^Germany.*", "Germany", st_prebivalcev$DRZAVA)

umrljivost_otrok <- read_csv("podatki/umrljivost_otrok.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
umrljivost_otrok <- umrljivost_otrok[, -c(3, 4, 5, 6, 7, 9)]
umrljivost_otrok <- umrljivost_otrok[-c(2, 35),]
names(umrljivost_otrok) <- (c("SPOL", "DRZAVA", "STEVILO SMRTI OTROK"))
umrljivost_otrok$DRZAVA <- gsub("^Germany.*", "Germany", umrljivost_otrok$DRZAVA)


umrljivost_otrok_na_100000 <- inner_join(st_prebivalcev, umrljivost_otrok, by="DRZAVA")
umrljivost_otrok_na_100000["STEVILO SMRTI OTROK NA 100.000 PREBIVALCEV"] <- umrljivost_otrok_na_100000["STEVILO SMRTI OTROK"] * 
  100000 / umrljivost_otrok_na_100000["STEVILO PREBIVALCEV"]

umrljivost_otrok_na_100000$SPOL[umrljivost_otrok_na_100000$SPOL == "Males"] <- "m"
umrljivost_otrok_na_100000$SPOL[umrljivost_otrok_na_100000$SPOL == "Females"] <- "f"

umrljivost_otrok_na_100000 <- umrljivost_otrok_na_100000[, -c(2, 4)]

zdravstveno_stanje <- inner_join(zdravstveno_stanje, umrljivost_otrok_na_100000, by=c("DRZAVA", "SPOL"))



### 2. TABELA

proracun <- read_csv("podatki/izdatki.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
proracun <- proracun[, -c(3, 4, 6)]
names(proracun) <- (c("LETO", "DRZAVA", "PRORACUN NA PREBIVALCA"))
proracun$DRZAVA <- gsub("^Germany.*", "Germany", proracun$DRZAVA)

st_postelj <- read_csv("podatki/st_postelj.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_postelj <- st_postelj[, -c(3, 4, 6)]
names(st_postelj) <- (c("LETO", "DRZAVA", "STEVILO POSTELJ"))
st_postelj$DRZAVA <- gsub("^Germany.*", "Germany", st_postelj$DRZAVA)

zdravstvo <- inner_join(proracun, st_postelj, by=c("LETO", "DRZAVA"))

st_operacijskih_sob <- read_csv("podatki/st_operacijskih_sob.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_operacijskih_sob <- st_operacijskih_sob[, -c(3, 4, 6)]
names(st_operacijskih_sob) <- (c("LETO", "DRZAVA", "STEVILO OPERACIJSKIH SOB"))
st_operacijskih_sob$DRZAVA <- gsub("^Germany.*", "Germany", st_operacijskih_sob$DRZAVA)

zdravstvo <- inner_join(zdravstvo, st_operacijskih_sob, by=c("LETO", "DRZAVA"))

st_osebja <- read_csv("podatki/st_zdravnikov_sester.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_osebja <- st_osebja[, -c(4, 6)]

st_zdravnikov <- filter(st_osebja, ISCO08 == "Medical doctors")
st_zdravnikov <- st_zdravnikov[, -c(1)]
names(st_zdravnikov) <- (c("LETO", "DRZAVA", "STEVILO ZDRAVNIKOV"))
st_zdravnikov$DRZAVA <- gsub("^Germany.*", "Germany", st_zdravnikov$DRZAVA)

zdravstvo <- inner_join(zdravstvo, st_zdravnikov, by=c("LETO", "DRZAVA"))

st_zdr_tehnikov <- filter(st_osebja, ISCO08 == "Nursing professionals and midwives")
st_zdr_tehnikov <- st_zdr_tehnikov[, -c(1)]
names(st_zdr_tehnikov) <- (c("LETO", "DRZAVA", "STEVILO ZDRAVSTVENIH TEHNIKOV"))
st_zdr_tehnikov$DRZAVA <- gsub("^Germany.*", "Germany", st_zdr_tehnikov$DRZAVA)

zdravstvo <- inner_join(zdravstvo, st_zdr_tehnikov, by=c("LETO", "DRZAVA"))

st_studentov <- read_csv("podatki/st_studentov.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_studentov <- st_studentov[, -c(4, 6)]

st_stud_medicine <- filter(st_studentov, ISCO08 == "Medical doctors")
st_stud_medicine <- st_stud_medicine[, -c(1)]
names(st_stud_medicine) <- (c("LETO", "DRZAVA", "STEVILO STUDENTOV MEDICINE"))
st_stud_medicine$DRZAVA <- gsub("^Germany.*", "Germany", st_stud_medicine$DRZAVA)

zdravstvo <- inner_join(zdravstvo, st_stud_medicine, by=c("LETO", "DRZAVA"))

st_stud_zdravstva <- filter(st_studentov, ISCO08 == "Nurses")
st_stud_zdravstva <- st_stud_zdravstva[, -c(1)]
names(st_stud_zdravstva) <- (c("LETO", "DRZAVA", "STEVILO STUDENTOV ZDRAVSTVA"))
st_stud_zdravstva$DRZAVA <- gsub("^Germany.*", "Germany", st_stud_zdravstva$DRZAVA)

zdravstvo <- inner_join(zdravstvo, st_stud_zdravstva, by=c("LETO", "DRZAVA"))


### 3. TABELA

BDP <- read_csv("podatki/BDP.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
BDP <- BDP[, -c(3, 4, 6)]
names(BDP) <- (c("LETO", "DRZAVA", "BDP"))
BDP$DRZAVA <- gsub("^Germany.*", "Germany", BDP$DRZAVA)

dohodek <- read_csv("podatki/placa.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
dohodek <- dohodek[, -c(1, 3, 4, 5, 7, 8, 9, 10, 11, 12, 14, 15)]
names(dohodek) <- (c("DRZAVA", "LETO", "DOHODEK"))
dohodek$DRZAVA <- gsub("^Germany.*", "Germany", dohodek$DRZAVA)

razvitost <- inner_join(BDP, dohodek, by=c("LETO", "DRZAVA"))

brezposelnost <- read_csv("podatki/brezposelnost.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
brezposelnost <- brezposelnost[, -c(3, 4, 5, 7)]
names(brezposelnost) <- (c("LETO", "DRZAVA", "STEVILO BREZPOSELNIH"))
brezposelnost$DRZAVA <- gsub("^Germany.*", "Germany", brezposelnost$DRZAVA)

razvitost <- inner_join(razvitost, brezposelnost, by=c("LETO", "DRZAVA"))

