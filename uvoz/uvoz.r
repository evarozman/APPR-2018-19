library(readr)
library(reshape2)
library(dplyr)

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

bolniska <- read_csv("podatki/bolniska.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
bolniska <- bolniska[, -c(1, 2, 3, 4, 5, 7, 8, 10, 11)]
bolniska["SPOL"] <- NA
names(bolniska) <- (c("DRŽAVA", "BOLNIŠKA", "SPOL"))

st_prebivalcev <- read_csv("podatki/st_prebiv.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
st_prebivalcev <- st_prebivalcev[, -c(1, 3, 5)]
names(st_prebivalcev) <- (c("DRŽAVA", "STEVILO PREBIVALCEV"))

umrljivost_otrok <- read_csv("podatki/umrljivost_otrok.csv", na=c(":", " : "), locale=locale(encoding="Windows-1250"))
umrljivost_otrok <- umrljivost_otrok[, -c(3, 4, 5, 6, 7, 9)]
names(umrljivost_otrok) <- (c("SPOL", "DRŽAVA", "ŠTEVILO SMRTI OTROK"))

