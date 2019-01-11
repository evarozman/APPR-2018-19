library(readr)
library(reshape2)
library(dplyr)



### 1. TABELA

zivljenska_doba <- read_csv("podatki/zivlj_doba.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
zivljenska_doba <- zivljenska_doba[, -c(3, 5)]

moski_zdrava_leta <- filter(zivljenska_doba, INDIC_HE == "Healthy life years in absolute value at birth - males")
moski_zdrava_leta["SPOL"] <- "m"
moski_zdrava_leta <- moski_zdrava_leta[, -c(1)]
names(moski_zdrava_leta) <- (c("DRŽAVA", "ZDRAVA LETA", "SPOL"))

zenske_zdrava_leta <- filter(zivljenska_doba, INDIC_HE == "Healthy life years in absolute value at birth - females")
zenske_zdrava_leta["SPOL"] <- "f"
zenske_zdrava_leta <- zenske_zdrava_leta[, -c(1)]
names(zenske_zdrava_leta) <- (c("DRŽAVA", "ZDRAVA LETA", "SPOL"))

moski_zivlj_doba <- filter(zivljenska_doba, INDIC_HE == "Life expectancy in absolute value at birth - males")
moski_zivlj_doba["SPOL"] <- "m"
moski_zivlj_doba <- moski_zivlj_doba[, -c(1)]
names(moski_zivlj_doba) <- (c("DRŽAVA", "ŽIVLJENSKA DOBA", "SPOL"))

zenske_zivlj_doba <- filter(zivljenska_doba, INDIC_HE == "Life expectancy in absolute value at birth - females")
zenske_zivlj_doba["SPOL"] <- "f"
zenske_zivlj_doba <- zenske_zivlj_doba[, -c(1)]
names(zenske_zivlj_doba) <- (c("DRŽAVA", "ŽIVLJENSKA DOBA", "SPOL"))

zdrava_leta <- rbind(moski_zdrava_leta, zenske_zdrava_leta)
zivlj_doba <- rbind(moski_zivlj_doba, zenske_zivlj_doba)

zdravstveno_stanje <- inner_join(zdrava_leta, zivlj_doba, by=c("DRŽAVA", "SPOL"))

st_prebivalcev <- read_csv("podatki/st_prebiv.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_prebivalcev <- st_prebivalcev[, -c(1, 3, 5)]
names(st_prebivalcev) <- (c("DRŽAVA", "ŠTEVILO PREBIVALCEV"))

umrljivost_otrok <- read_csv("podatki/umrljivost_otrok.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
umrljivost_otrok <- umrljivost_otrok[, -c(3, 4, 5, 6, 7, 9)]
umrljivost_otrok <- umrljivost_otrok[-c(2, 35),]

names(umrljivost_otrok) <- (c("SPOL", "DRŽAVA", "ŠTEVILO SMRTI OTROK"))

umrljivost_otrok_na_100000 <- inner_join(st_prebivalcev, umrljivost_otrok, by="DRŽAVA")
umrljivost_otrok_na_100000["ŠTEVILO SMRTI OTROK NA 100.000 PREBIVALCEV"] <- umrljivost_otrok_na_100000["ŠTEVILO SMRTI OTROK"] * 
  100000 / umrljivost_otrok_na_100000["ŠTEVILO PREBIVALCEV"]

umrljivost_otrok_na_100000$SPOL[umrljivost_otrok_na_100000$SPOL == "Males"] <- "m"
umrljivost_otrok_na_100000$SPOL[umrljivost_otrok_na_100000$SPOL == "Females"] <- "f"

umrljivost_otrok_na_100000 <- umrljivost_otrok_na_100000[, -c(2, 4)]

zdravstveno_stanje <- inner_join(zdravstveno_stanje, umrljivost_otrok_na_100000, by=c("DRŽAVA", "SPOL"))



### 2. TABELA

proracun <- read_csv("podatki/izdatki.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
proracun <- proracun[, -c(3, 4, 6)]
names(proracun) <- (c("LETO", "DRŽAVA", "PRORAČUN NA PREBIVALCA"))

st_postelj <- read_csv("podatki/st_postelj.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_postelj <- st_postelj[, -c(3, 4, 6)]
names(st_postelj) <- (c("LETO", "DRŽAVA", "ŠTEVILO POSTELJ"))

zdravstvo <- inner_join(proracun, st_postelj, by=c("LETO", "DRŽAVA"))

st_operacijskih_sob <- read_csv("podatki/st_operacijskih_sob.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_operacijskih_sob <- st_operacijskih_sob[, -c(3, 4, 6)]
names(st_operacijskih_sob) <- (c("LETO", "DRŽAVA", "ŠTEVILO OPERACIJSKIH SOB"))

zdravstvo <- inner_join(zdravstvo, st_operacijskih_sob, by=c("LETO", "DRŽAVA"))

st_osebja <- read_csv("podatki/st_zdravnikov_sester.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_osebja <- st_osebja[, -c(4, 6)]

st_zdravnikov <- filter(st_osebja, ISCO08 == "Medical doctors")
st_zdravnikov <- st_zdravnikov[, -c(1)]
names(st_zdravnikov) <- (c("LETO", "DRŽAVA", "ŠTEVILO ZDRAVNIKOV"))

zdravstvo <- inner_join(zdravstvo, st_zdravnikov, by=c("LETO", "DRŽAVA"))

st_zdr_tehnikov <- filter(st_osebja, ISCO08 == "Nursing professionals and midwives")
st_zdr_tehnikov <- st_zdr_tehnikov[, -c(1)]
names(st_zdr_tehnikov) <- (c("LETO", "DRŽAVA", "ŠTEVILO ZDRAVSTVENIH TEHNIKOV"))

zdravstvo <- inner_join(zdravstvo, st_zdr_tehnikov, by=c("LETO", "DRŽAVA"))

st_studentov <- read_csv("podatki/st_studentov.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_studentov <- st_studentov[, -c(4, 6)]

st_stud_medicine <- filter(st_studentov, ISCO08 == "Medical doctors")
st_stud_medicine <- st_stud_medicine[, -c(1)]
names(st_stud_medicine) <- (c("LETO", "DRŽAVA", "ŠTEVILO ŠTUDENTOV MEDICINE"))

zdravstvo <- inner_join(zdravstvo, st_stud_medicine, by=c("LETO", "DRŽAVA"))

st_stud_zdravstva <- filter(st_studentov, ISCO08 == "Nurses")
st_stud_zdravstva <- st_stud_zdravstva[, -c(1)]
names(st_stud_zdravstva) <- (c("LETO", "DRŽAVA", "ŠTEVILO ŠTUDENTOV ZDRAVSTVA"))

zdravstvo <- inner_join(zdravstvo, st_stud_zdravstva, by=c("LETO", "DRŽAVA"))


### 3. TABELA

BDP <- read_csv("podatki/BDP.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
BDP <- BDP[, -c(3, 4, 6)]
names(BDP) <- (c("LETO", "DRŽAVA", "BDP"))

dohodek <- read_csv("podatki/placa.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
dohodek <- dohodek[, -c(1, 3, 4, 5, 7, 8, 9, 10, 11, 12, 14, 15)]
names(dohodek) <- (c("DRŽAVA", "LETO", "DOHODEK"))

razvitost <- inner_join(BDP, dohodek, by=c("LETO", "DRŽAVA"))

brezposelnost <- read_csv("podatki/brezposelnost.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
brezposelnost <- brezposelnost[, -c(3, 4, 5, 7)]
names(brezposelnost) <- (c("LETO", "DRŽAVA", "ŠTEVILO BREZPOSELNIH"))

razvitost <- inner_join(razvitost, brezposelnost, by=c("LETO", "DRŽAVA"))

