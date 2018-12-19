# Analiza podatkov s programom R, 2018/19

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2018/19

* [![Shiny](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/evarozman/APPR-2018-19/master?urlpath=shiny/APPR-2018-19/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/evarozman/APPR-2018-19/master?urlpath=rstudio) RStudio

## Analiza zdravstva v Evropi

V projektu bom analizirala glavne kazalce zdravstvenega stanja držav Evropske unije.

Prvo področje bo splošno zdravstveno stanje leta 2014, kjer se bom osredotočila na podatke o pričakovani življenjski dobi, bolniški odsotnosti z dela, ljudi s psihološkimi boleznimi in umrljivost otrok do enega leta starosti.

Drugo področje je zdravstvo kot institucija, kjer bom vključila podatke o proračunu, ki je namenjen za zdravstvo na prebivalca, število mest v zdravstvenih ustanovah in število zdravstevih delavcev glede na število prebivalcev in pa koliko ljudi in pa v kolikšni meri potrebuje oskrbo na domu.

Tretje področje je razvitost države. Tukaj bom raziskala gospodarsko rast, s poudarkom na rasti bruto domačega proizvoda na prebivalca, na dohodek in brezposelnost.

Moj cilj je poiskati povezave med trenutnim zdravstvenim stanjem posamezne države, vlaganjem v zdravstvo in razvitosjo oziroma razvojem države.

Za vir podatkov bom uporabila Eurostat (https://ec.europa.eu/eurostat/data/database), Wikipedijo (https://sl.wikipedia.org/wiki/Pri%C4%8Dakovana_%C5%BEivljenjska_doba) in World Bank (http://databank.worldbank.org/data/source/world-development-indicators).

TABELE:
1. tabela (zdravstveno stanje): 
	* država
	* spol
	* pričakovana življenjska doba
	* število zdravih let
	* bolniška odsotnost od dela
	* umrljivost otrok do 1 leta starosti

2. tabela (zdravstvo):
	* država
	* proračun na prebivalca
	* število mest v zdravstvenih ustanovah (na 100.000 prebivalcev)
	* število zdravstvenih delavcev (na 1.000 prebivalcev)
	* število ljudi z oskrbo na domu

3. tabela (razvitost):
	* država
	* BDP
	* dohodek
	* brezposelnost

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-201819)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem.zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
