# ---- Analyse ----
# Dit is de statistiche code voor ZetelRaming met de datasets zoals die te 
# downloaden zijn op ZetelRaming.nl. Voor de Bayesian analysis moet ook nog
# bestanden met .stan gedownloadt worden. Stuur gerust vragen over de code of 
# bug reports naar contact@zetelraming.nl .
# 
# In dit bestand worden peilingen waarvan alleen zetels bekend zijn naar  
# percentages omgezet, de huiseffecten berekend, peilinggemiddeldes uitgerekend,
# en het model toegepast. Op https://zetelraming.nl/verantwoording leg ik uit
# welke keuzes ik waarom gemaakt heb in het ontwerpen van het model.
# 
# De code is geschreven in R, en maakt veel gebruik van functies en de '%>%' 
# operator uit het TidyVerse package.p

setwd("/dataproject/Peilingen")              # Pas dit pad aan naar waar u de 
library("tidyverse")                         # datasets heeft opgeslagen.
options(mc.cores = parallel::detectCores())
library(lubridate)
library(quadprog)
library("rstan")
library(shinystan)
library(ggthemes)
 

#Data laden
Peilingen = readRDS("Peilingen.RDS")
Uitslagen = readRDS("Uitslagen.RDS")

# ---- Zetels naar percentage omzetten ----
# De verhouding hangt af van hoeveel stemmen er gaan naar partijen die 
# uiteindelijk geen zetel winnen. Omdat peilers percentages meten en naar zetels 
# omzetten gebruik ik peilingen waarvan ik zowel de zetels als percentages heb 
# op de verhouding uit te rekenen, en hoop ik dat peilers allemaal ongeveer 
# dezelfde verhouding gebruiken.

coefficients = Peilingen %>% 
  select(Partij, Zetels, Percentage, Publicatie) %>% na.omit()

coefficients = lm(Percentage ~ Zetels , data = coefficients) %>% summary()

# Verhouding implementeren
Peilingen$Percentage[is.na(Peilingen$Percentage)] = 0

Peilingen$Percentage = if_else(Peilingen$Percentage == 0, 
  Peilingen$Zetels * coefficients$coefficients[2, 1] +
  coefficients$coefficients[1, 1], Peilingen$Percentage)
 
# ---- Huiseffecten ----
# Huiseffecten interpreteer ik als  hoeveel een peiler gemiddeld afwijkt van
# van wat de gemiddelde peiler meet. De code om dat uit te rekenen is wat
# omslachtig, maar werkt prima met de summarise functie om gemiddeldes uit te 
# rekenen. Huiseffecten veranderen wanneer er meer peilingen beschikbaar komen,
# en de gemiddeldes vanaf 2012 veranderen daarom ook met nieuwe peilingdata.

Peilingen = Peilingen %>% 
  unite(PeilsterPlus, Peilster, Partij, 
    NaoorlogseVerkiezingsNummer, remove = FALSE)

sum = Peilingen %>% group_by(PeilsterPlus) %>%
  drop_na(Percentage) %>% 
  summarise(mean = mean(Percentage))

sum = sum %>% 
  separate(PeilsterPlus, c("Peilster", "Partij", "Verkiezingsnummer"), 
    sep = "_")

sum1 = sum %>% 
  unite(PartijVerkiezing, Partij, Verkiezingsnummer, remove = FALSE) %>% 
  group_by(PartijVerkiezing) %>%
  summarise(a = mean(mean)) %>%
  separate(PartijVerkiezing, c( "Partij", "Verkiezingsnummer"), sep = "_")

sum = sum %>% 
  left_join(sum1, by = c("Partij", "Verkiezingsnummer")) %>% 
  mutate(Huiseffect = mean - a)

Peilingen = sum %>% 
  unite(PeilsterPlus, Peilster, Partij, Verkiezingsnummer) %>% 
  select(PeilsterPlus, Huiseffect) %>% 
  right_join(Peilingen, by = "PeilsterPlus") %>%
  mutate(PeilsterPlus = NULL, 
    HuisPercentage = Percentage - Huiseffect,
    Huiseffect = NULL, 
    PeilingWeek = NULL)

# ---- Hoeveel voorspellender worden peilingen dichter bij de verkiezingen? ---- 
# Om dit uit te rekenen had ik per partij voor iedere peiling nodig wat het
# percentage bij de vorige peiling was. Ik koos om dit zonder for loop of 
# apply function, en daarom is de code wat omslachtig met het tijdelijk omzetten
# van NA in een negatief getal.

Peilingen = Peilingen %>% 
  arrange(Partij, Publicatie) %>% 
  mutate(PartijLag = Partij, 
    DagenVorige = lag(DagenTotVerkiezing) - DagenTotVerkiezing,
    )

Peilingen$PartijLag = if_else(Peilingen$PartijLag == lag(Peilingen$Partij), 
  lag(Peilingen$HuisPercentage), -1)

Peilingen$PartijLag = na_if(Peilingen$PartijLag, -1.0000)

Peilingen$DagenVorige = if_else(Peilingen$Partij == lag(Peilingen$Partij), 
  Peilingen$DagenVorige, -1.1)

Peilingen$DagenVorige = na_if(Peilingen$DagenVorige, -1.1)

Peilingen$Partij = as_factor(Peilingen$Partij)

#Helaas werken zonder deze regel logaritmes niet want ln(0) = -inf
Peilingen$DagenTotVerkiezing[Peilingen$DagenTotVerkiezing == 0] = 1 

Peilingen = Peilingen %>% 
  mutate(Verschil = StemPercentage - HuisPercentage, 
    VerschilVorige = StemPercentage - PartijLag,
         VerschilBeideKanten = sqrt(Verschil ^ 2),
         VerschilBeideKantenVorige = sqrt(VerschilVorige ^ 2),
         Campagne = DagenTotVerkiezing <= 30,
         lnDagenTotVerkiezing = log(as.numeric(Peilingen$DagenTotVerkiezing)))

Peilingen = Peilingen %>% 
  unite(VerkiezingsPartij, Verkiezingsdag, Partij, remove = FALSE)

sum3 = lm(VerschilBeideKanten ~ VerschilBeideKantenVorige + 
    lnDagenTotVerkiezing + DagenTotVerkiezing, data = Peilingen) %>% 
  summary()  # Typ sum3 in de console om de resultaten te zien

# ---- Peilinggemiddeldes per dag uirekenen ----
# Bevat een grote for loop die op langzame computers lang kan duren omdat hij
# niet volledig efficient is geschreven. En dan dezelfde twee keer, 1 keer voor
# de peilingen verzameld door PeilingWijzer en ZetelRaming, en 1 keer voor de
# peilingen van tot 2002.

# Deze functie zorgt ervoor dat voor iedere dag de dichtsbijzijnde peiling wordt
# gevonden per partij, zolang die partij in de laatste 100 dagen in een peiling
# is verschenen.
dichstbij <- function(joop, ...){
  b = peilset$dag
  b = b[b <= joop]
  if (joop - max(b) < 100) {
    return(max(b))
  }
  else{
    return(NA)
  }
}

# Hoe sterk peilingen gewogen worden hangt af van hoeveel voorspellender ze zijn
# dichter bij de verkiezing volgens de vergelijking in sum3. In de praktijk weegt
# dit niet zwaar genoeg om oude peilingen snel een weging van 0 te geven. Daarom kan
# er een maximum worden ingesteld, met de variabel max. Zie het bestand werkscript,
# voor de code die gebruikt is om max te bepalen.
# Dmat, bvec dvec en amat zijn de matrices en vectoren die nodig zijn voor de functie
# quadprog. Dmat zijn de constanten in de te optimaliseren formule, met dvec de  
# speciefieke waardes. Amat en bvec zijn samen de constraints.
max = 5

Dmat = diag(rep(2 * sum3[["sigma"]] ^ 2, max))

Amat = cbind(rep(1, max), diag(rep(1, max)))

bvec = c(1, rep(0, max))

Gemiddeldes = tibble(today(), 2.1, "Partij", 
  .rows = 0)
colnames(Gemiddeldes) = c("Publicatie", "Percentage", "Partij")

Peilingen = Peilingen %>% mutate(Partij = as.character(Partij)) %>%
 filter( Partij != "BBB", Partij != "NA", Partij != "Overig")

for (e in unique(Peilingen$Partij)) {
  peilset = Peilingen %>% 
    filter(Partij == e) %>% 
    select(dag = Publicatie, 
      dagentotverkiezing = DagenTotVerkiezing,
      percentages = HuisPercentage, Partij) %>%
    mutate(ZelfdeDag = lead(duplicated(dag))) %>% 
    mutate(dag = as.numeric(dag),
      dagentotverkiezing = as.numeric(dagentotverkiezing))
 
  # Vorige peilingen linken per peiling, en daarna voor iedere dag de 
  # meest recente peiling vinden.
  
  publicatie = matrix(peilset$dag)
  
  percentages = matrix(peilset$percentages)
  
  for (i in 1:(max - 1)) {
    
    publicatie = cbind(publicatie, lag(peilset$dag, n = i))
    
    percentages = cbind(percentages, lag(peilset$percentages, n = i))
  }
  
  peilset = peilset %>% 
    mutate(dag = NULL, percentages = NULL, Partij = NULL) %>%
    cbind(publicatie, percentages)
  
  colnames(peilset) = c("dagentotverkiezing", "ZelfdeDag", "dag", 1:(2 * max - 1))
  
  peilset = peilset %>%
    filter(ZelfdeDag == FALSE) %>%
    mutate(ZelfdeDag = NULL) 
  
  Datums = tibble(Dagen = (as.numeric(ymd("1964-1-1")):(as.numeric(today()))),
                  dag = length(Dagen))
  
  Datums$dag = sapply(X = Datums$Dagen,   
    FUN = dichstbij, peilset)
  
  Datums = left_join(Datums, peilset, by = "dag") %>% 
    mutate(across(c("dag", 4:(max + 2)), function(x) Dagen - x +
        dagentotverkiezing)) %>%
    mutate(across(c("dag", 4:(max + 2)), function(x) sum3$coefficients[1] + 
        sum3$coefficients[4] * x + sum3$coefficients[3] * log(x)))
  
  dvecs = Datums %>% 
    na.omit() %>%
    select(c("dag", 4:(max + 2)))
  
  oplossing = dvecs
  
  for (i in 1:nrow(dvecs)) { 
    dvec = dvecs[i, ]
    qp <- solve.QP(Dmat, -(dvec), Amat, bvec, meq = 1)
    oplossing[i, ] = as.list(qp$solution)
  }
  percentages = Datums %>%
    na.omit() %>%
    select(c((max + 3):(2 * max + 2)))
  
  Gemiddelde = Datums %>% 
    na.omit() %>% 
    select(Dagen) %>% 
    cbind(rowSums( percentages * oplossing)) %>%
    mutate(Dagen = as_date(Dagen), Partij = e)
  
  Gemiddelde$Partij = e
  
  colnames(Gemiddelde) = c("Publicatie", "Percentage", "Partij")
  
  Gemiddeldes = rbind(Gemiddeldes, Gemiddelde)
}

# Verkiezingsdag weer invoegen
Gemiddeldes$Verkiezingsdag = case_when(Gemiddeldes$Publicatie > ymd("2017-03-15") ~ ymd("2021-03-17"),
  Gemiddeldes$Publicatie > ymd("2012-09-12") ~ ymd("2017-03-15"),
  Gemiddeldes$Publicatie > ymd("2010-06-09") ~ ymd("2012-09-12"),
  Gemiddeldes$Publicatie > ymd("2006-11-22") ~ ymd("2010-06-09"),
  Gemiddeldes$Publicatie > ymd("2003-01-22") ~ ymd("2006-11-22"),
  Gemiddeldes$Publicatie > ymd("2002-05-15") ~ ymd("2003-01-22"),
  Gemiddeldes$Publicatie > ymd("1998-05-06") ~ ymd("2002-05-15"),
  Gemiddeldes$Publicatie > ymd("1994-05-03") ~ ymd("1998-05-06"),
  Gemiddeldes$Publicatie > ymd("1989-09-06") ~ ymd("1994-05-03"),
  Gemiddeldes$Publicatie > ymd("1986-05-21") ~ ymd("1989-09-06"),
  Gemiddeldes$Publicatie > ymd("1982-09-08") ~ ymd("1986-05-21"),
  Gemiddeldes$Publicatie > ymd("1981-05-26") ~ ymd("1982-09-08"),
  Gemiddeldes$Publicatie > ymd("1977-05-25") ~ ymd("1981-05-26"),
  Gemiddeldes$Publicatie > ymd("1972-11-29") ~ ymd("1977-05-25"),
  Gemiddeldes$Publicatie > ymd("1971-04-28") ~ ymd("1972-11-29"),
  Gemiddeldes$Publicatie > ymd("1967-02-15") ~ ymd("1971-04-28"),
  Gemiddeldes$Publicatie > ymd("1963-05-15") ~ ymd("1967-02-15"),
  TRUE ~ ymd("1963-05-15"))

# ---- Bayesian Analysis met Stan ----
# Uitslagen uitgelegd door onderliggende factoren. Deze worden dan later
# met peilingen gecombineert voor de verkiezingsvoorspelling

Uitslagen = Uitslagen %>%
  mutate(Premier_GDP = GDPgroei * Premier, Regering_GDP = InRegering * GDPgroei,
    Premier_Werkloos = Premier * `Beroepsbevolking|Werkloosheidspercentage`,
    Regering_Werkloos = InRegering * `Beroepsbevolking|Werkloosheidspercentage`)

Covariates = tibble(variabel = 
  c("StemPercentageVorige", "Premier", "MinisterPresident", "InRegering", 
    "KabinetGevallen", "Uitgestapt", "VerkiezingenLijsttrekker", "Minister",
    "Vrouw", "OoitMinister", "GDPgroei", 
    "Beroepsbevolking|Werkloosheidspercentage", "Regering_Werkloos", 
    "Regering_GDP","Premier_GDP", 
    "Premier_Werkloos", "NaoorlogseVerkiezingsNummer" ))

StanUitslag = Uitslagen %>% 
  select(StemPercentage, Partij, Verkiezingsdag, c(Covariates$variabel)) %>% 
  na.omit()
 
StanUitslagen =  list(N = nrow(StanUitslag),
  K = ncol(StanUitslag) - 3, x = as.matrix(select(StanUitslag, 
    (!c("StemPercentage", "Partij", "Verkiezingsdag")))),
  y = c(StanUitslag$StemPercentage))

# Partijen worden in twee ongeveer even grote groepen gesplitst, kleine en grote
# partijen bij de vorige verkiezing omdat kleine partijen licht anders reageren op
# veranderingen in onderliggende factoren. Dit zorgt wel voor dat veel code er
# drie keer staat

StanUitslag2 = StanUitslag %>% filter(StemPercentageVorige >= 5.2)
StanUitslag3 = StanUitslag %>% filter(StemPercentageVorige < 5.2)

StanUitslagen2 =  list(N = nrow(StanUitslag2),
  K = ncol(StanUitslag2) - 3, x = as.matrix(select(StanUitslag2, 
    (!c("StemPercentage", "Partij", "Verkiezingsdag")))),
  y = c(StanUitslag2$StemPercentage))
StanUitslagen3 =  list(N = nrow(StanUitslag3),
  K = ncol(StanUitslag3) - 3, x = as.matrix(select(StanUitslag3, 
    (!c("StemPercentage", "Partij", "Verkiezingsdag")))),
  y = c(StanUitslag3$StemPercentage))

fit1 = stan(file = "model1.stan", data = StanUitslagen)
fit2 = stan(file = "model1.stan", data = StanUitslagen2)
fit3 = stan(file = "model1.stan", data = StanUitslagen3)

exfit = tibble(Variabel = c("alpha", Covariates$variabel, "sigma", "lp_"), 
  Normaal = colMeans(as.data.frame(rstan::extract(fit1))),
  Grote = colMeans(as.data.frame(rstan::extract(fit2))),
  Kleine = colMeans(as.data.frame(rstan::extract(fit3))))

StanUitslag$Predicted = as.numeric(exfit[1,2]) +
  as.numeric(exfit[2,2]) * StanUitslag[, 4] + as.numeric(exfit[3,2]) * StanUitslag[, 5] + 
  as.numeric(exfit[4,2]) * StanUitslag[, 6] + as.numeric(exfit[5,2]) * StanUitslag[, 7] +
  as.numeric(exfit[6,2]) * StanUitslag[, 8] + as.numeric(exfit[7,2]) * StanUitslag[, 9] + 
  as.numeric(exfit[8,2]) * StanUitslag[, 10] + as.numeric(exfit[9,2]) * StanUitslag[, 11] + 
  as.numeric(exfit[10,2]) * StanUitslag[, 12] + as.numeric(exfit[11, 2]) * StanUitslag[, 13] + 
  as.numeric(exfit[12,2]) * StanUitslag[, 14] + as.numeric(exfit[13,2]) * StanUitslag[, 15] +
  as.numeric(exfit[14,2]) * StanUitslag[, 16] + as.numeric(exfit[15,2]) * StanUitslag[, 17] + 
  as.numeric( exfit[16,2]) * StanUitslag[, 18] + as.numeric(exfit[17,2]) * StanUitslag[, 19] +
  as.numeric(exfit[18,2]) * StanUitslag[, 20]

StanUitslag = mutate(StanUitslag, Predicted = Predicted$StemPercentageVorige)

StanUitslag2$Predicted = as.numeric(exfit[1,3]) +
  as.numeric(exfit[2,3]) * StanUitslag2[, 4] + as.numeric(exfit[3,3]) * StanUitslag2[, 5] + 
  as.numeric(exfit[4,3]) * StanUitslag2[, 6] + as.numeric(exfit[5,3]) * StanUitslag2[, 7] +
  as.numeric(exfit[6,3]) * StanUitslag2[, 8] + as.numeric(exfit[7,3]) * StanUitslag2[, 9] + 
  as.numeric(exfit[8,3]) * StanUitslag2[, 10] + as.numeric(exfit[9,3]) * StanUitslag2[, 11] + 
  as.numeric(exfit[10,3]) * StanUitslag2[, 12] + as.numeric(exfit[11, 3]) * StanUitslag2[, 13] +
  as.numeric(exfit[12,3]) * StanUitslag2[, 14] + as.numeric(exfit[13,3]) * StanUitslag2[, 15] +
  as.numeric(exfit[14,3]) * StanUitslag2[, 16] + as.numeric(exfit[15,3]) * StanUitslag2[, 17] +
  as.numeric(exfit[16,3]) * StanUitslag2[, 18] + as.numeric(exfit[17,3]) * StanUitslag2[, 19] +
  as.numeric(exfit[18,3]) * StanUitslag2[, 20]

StanUitslag2 = mutate(StanUitslag2, Predicted = Predicted$StemPercentageVorige)
  
StanUitslag3$Predicted = as.numeric(exfit[1,4]) +
  as.numeric(exfit[2,4]) * StanUitslag3[, 4] + as.numeric(exfit[3,4]) * StanUitslag3[, 5] + 
  as.numeric(exfit[4,4]) * StanUitslag3[, 6] + as.numeric(exfit[5,4]) * StanUitslag3[, 7] +
  as.numeric(exfit[6,4]) * StanUitslag3[, 8] + as.numeric(exfit[7,4]) * StanUitslag3[, 9] + 
  as.numeric(exfit[8,4]) * StanUitslag3[, 10] + as.numeric(exfit[9,4]) * StanUitslag3[, 11] + 
  as.numeric(exfit[10,4]) * StanUitslag3[, 12] + as.numeric(exfit[11, 4]) * StanUitslag3[, 13] +
  as.numeric(exfit[12,4]) * StanUitslag3[, 14] + as.numeric(exfit[13,4]) * StanUitslag3[, 15] +
  as.numeric(exfit[14,4]) * StanUitslag3[, 16] + as.numeric(exfit[15,4]) * StanUitslag3[, 17] + 
  as.numeric(exfit[16,4]) * StanUitslag3[, 18] + as.numeric(exfit[17,4]) * StanUitslag3[, 19] + 
  as.numeric(exfit[18,4]) * StanUitslag3[, 20]
StanUitslag3 = mutate(StanUitslag3, Predicted = Predicted$StemPercentageVorige)

Gemiddeldes1 = Gemiddeldes %>% 
  mutate(
    DagenTotVerkiezingen = as.numeric(Verkiezingsdag) - as.numeric(Publicatie)) %>% 
  left_join(StanUitslag, by = c("Verkiezingsdag", "Partij")) %>%
  mutate(PeilingVerschil = Percentage - StemPercentageVorige) %>% 
  select(Percentage, DagenTotVerkiezingen, Predicted, StemPercentage) %>%
  na.omit() %>% 
  mutate(PeilingDagen = Percentage * DagenTotVerkiezingen,
  PredictedDagen = Predicted * DagenTotVerkiezingen)

Gemiddeldes2 = Gemiddeldes %>% 
  mutate(
    DagenTotVerkiezingen = as.numeric(Verkiezingsdag) - as.numeric(Publicatie)) %>% 
  left_join(StanUitslag2, by = c("Verkiezingsdag", "Partij")) %>%
  filter(StemPercentageVorige >= 5.2) %>%
  mutate(PeilingVerschil = Percentage - StemPercentageVorige) %>% 
  select(Percentage, DagenTotVerkiezingen, Predicted, StemPercentage) %>%
  na.omit() %>% 
  mutate(PeilingDagen = Percentage * DagenTotVerkiezingen,
    PredictedDagen = Predicted * DagenTotVerkiezingen) 

Gemiddeldes3 = Gemiddeldes %>% 
  mutate(
    DagenTotVerkiezingen = as.numeric(Verkiezingsdag) - as.numeric(Publicatie)) %>% 
  left_join(StanUitslag3, by = c("Verkiezingsdag", "Partij")) %>%
  filter(StemPercentageVorige < 5.2) %>%
  mutate(PeilingVerschil = Percentage - StemPercentageVorige) %>% 
  select(Percentage, DagenTotVerkiezingen, Predicted, StemPercentage) %>%
  na.omit() %>% 
  mutate(PeilingDagen = Percentage * DagenTotVerkiezingen,
    PredictedDagen = Predicted * DagenTotVerkiezingen) 

Gemiddeldes2 = rbind(Gemiddeldes2, Gemiddeldes3)

# --- Verhouding peilingen en onderliggende factoren ---
# Sum4 en sum5 bevatten de regressies die bepalen hoe sterk de peilingen en
# onderliggende factoren worden meegewogen, zodat verkiezingen tot nu toe het
# best voorspeld zouden zijn door het model.

sum4 = lm(StemPercentage ~ Percentage + Predicted, data = Gemiddeldes1) %>% summary()
sum5 = lm(StemPercentage ~ Percentage + Predicted, data = Gemiddeldes2) %>% summary()


TeVoorspellenUitslagen = Uitslagen %>% 
  select(StemPercentage, Partij, Verkiezingsdag, c(Covariates$variabel)) %>%
  filter(Verkiezingsdag == ymd("2021-3-17"))

TeVoorspellenUitslagen$StemPercentageVorige[
  TeVoorspellenUitslagen$Partij == "JA21" | 
    TeVoorspellenUitslagen$Partij == "Code Oranje" | 
    TeVoorspellenUitslagen$Partij == "BIJ1" |  
    TeVoorspellenUitslagen$Partij == "Volt" ] = 0

TeVoorspellenUitslagen$Predicted = as.numeric(exfit[1,2]) +
  as.numeric(exfit[2,2]) * TeVoorspellenUitslagen[, 4] + 
  as.numeric(exfit[3,2]) * TeVoorspellenUitslagen[, 5] + 
  as.numeric(exfit[4,2]) * TeVoorspellenUitslagen[, 6] + 
  as.numeric(exfit[5,2]) * TeVoorspellenUitslagen[, 7] +
  as.numeric(exfit[6,2]) * TeVoorspellenUitslagen[, 8] + 
  as.numeric(exfit[7,2]) * TeVoorspellenUitslagen[, 9] + 
  as.numeric(exfit[8,2]) * TeVoorspellenUitslagen[, 10] + 
  as.numeric(exfit[9,2]) * TeVoorspellenUitslagen[, 11] + 
  as.numeric(exfit[10,2]) * TeVoorspellenUitslagen[, 12] + 
  as.numeric(exfit[11, 2]) * TeVoorspellenUitslagen[, 13] + 
  as.numeric(exfit[12,2]) * TeVoorspellenUitslagen[, 14] + 
  as.numeric(exfit[13,2]) * TeVoorspellenUitslagen[, 15] +
  as.numeric(exfit[14,2]) * TeVoorspellenUitslagen[, 16] + 
  as.numeric(exfit[15,2]) * TeVoorspellenUitslagen[, 17] + 
  as.numeric(exfit[16,2]) * TeVoorspellenUitslagen[, 18] + 
  as.numeric(exfit[17,2]) * TeVoorspellenUitslagen[, 19] +
  as.numeric(exfit[18,2]) * TeVoorspellenUitslagen[, 20]

TeVoorspellenUitslagen = mutate(TeVoorspellenUitslagen, Predicted = Predicted$StemPercentageVorige)

TeVoorspellenUitslagen$Predicted2 = as.numeric(exfit[1,3]) +
  as.numeric(exfit[2,3]) * TeVoorspellenUitslagen[, 4] + 
  as.numeric(exfit[3,3]) * TeVoorspellenUitslagen[, 5] + 
  as.numeric(exfit[4,3]) * TeVoorspellenUitslagen[, 6] +
  as.numeric(exfit[5,3]) * TeVoorspellenUitslagen[, 7] +
  as.numeric(exfit[6,3]) * TeVoorspellenUitslagen[, 8] + 
  as.numeric(exfit[7,3]) * TeVoorspellenUitslagen[, 9] + 
  as.numeric(exfit[8,3]) * TeVoorspellenUitslagen[, 10] + 
  as.numeric(exfit[9,3]) * TeVoorspellenUitslagen[, 11] + 
  as.numeric(exfit[10,3]) * TeVoorspellenUitslagen[, 12] + 
  as.numeric(exfit[11,3]) * TeVoorspellenUitslagen[, 13] +
  as.numeric(exfit[12,3]) * TeVoorspellenUitslagen[, 14] + 
  as.numeric(exfit[13,3]) * TeVoorspellenUitslagen[, 15] +
  as.numeric(exfit[14,3]) * TeVoorspellenUitslagen[, 16] + 
  as.numeric(exfit[15,3]) * TeVoorspellenUitslagen[, 17] +
  as.numeric(exfit[16,3]) * TeVoorspellenUitslagen[, 18] + 
  as.numeric(exfit[17,3]) * TeVoorspellenUitslagen[, 19] +
  as.numeric(exfit[18,3]) * TeVoorspellenUitslagen[, 20] 

TeVoorspellenUitslagen = mutate(TeVoorspellenUitslagen, Predicted2 = Predicted2$StemPercentageVorige)

TeVoorspellenUitslagen$Predicted3 = as.numeric(exfit[1,4]) +
  as.numeric(exfit[2,4]) * TeVoorspellenUitslagen[, 4] + 
  as.numeric(exfit[3,4]) * TeVoorspellenUitslagen[, 5] + 
  as.numeric(exfit[4,4]) * TeVoorspellenUitslagen[, 6] + 
  as.numeric(exfit[5,4]) * TeVoorspellenUitslagen[, 7] +
  as.numeric(exfit[6,4]) * TeVoorspellenUitslagen[, 8] + 
  as.numeric(exfit[7,4]) * TeVoorspellenUitslagen[, 9] + 
  as.numeric(exfit[8,4]) * TeVoorspellenUitslagen[, 10] + 
  as.numeric(exfit[9,4]) * TeVoorspellenUitslagen[, 11] + 
  as.numeric(exfit[10,4]) * TeVoorspellenUitslagen[, 12] +
  as.numeric(exfit[11, 4]) * TeVoorspellenUitslagen[, 13] +
  as.numeric(exfit[12,4]) * TeVoorspellenUitslagen[, 14] + 
  as.numeric(exfit[13,4]) * TeVoorspellenUitslagen[, 15] +
  as.numeric(exfit[14,4]) * TeVoorspellenUitslagen[, 16] + 
  as.numeric(exfit[15,4]) * TeVoorspellenUitslagen[, 17] + 
  as.numeric(exfit[16,4]) * TeVoorspellenUitslagen[, 18] + 
  as.numeric(exfit[17,4]) * TeVoorspellenUitslagen[, 19] + 
  as.numeric(exfit[18,4]) * TeVoorspellenUitslagen[, 20]
TeVoorspellenUitslagen = mutate(TeVoorspellenUitslagen, Predicted3 = Predicted3$StemPercentageVorige)

TeVoorspellen = Gemiddeldes %>% 
  filter(Verkiezingsdag == ymd("2021-3-17")) %>% 
  select(Partij, Dag = Publicatie, Percentage, Verkiezingsdag) %>% 
  left_join(TeVoorspellenUitslagen, by = c("Partij", "Verkiezingsdag")) %>% 
  mutate(PredictedNieuw = if_else(StemPercentageVorige >= 5.2, Predicted2, Predicted3)) %>%
  select(Partij, Dag, Percentage, Predicted, PredictedNieuw) %>% 
  mutate(ZetelramingPercentage = sum4$coefficients[1] + 
   Percentage * sum4$coefficients[2] + 
   Predicted  * sum4$coefficients[3], 
   ZetelRaming = ZetelramingPercentage / coefficients$coefficients[2, 1] - coefficients$coefficients[1, 1],
  ZetelramingPercentage2 = sum5$coefficients[1] + 
      Percentage * sum5$coefficients[2] + 
      PredictedNieuw  * sum5$coefficients[3], 
    ZetelRaming2 = ZetelramingPercentage2 / coefficients$coefficients[2, 1] - coefficients$coefficients[1, 1]
    )

# Omdat nieuwe partijen niet betrouwbaar kunnen worden meegenomen met onderliggende factoren,
# wordt voor deze partijen het percentage uit de peilingen gebruikt.
TeVoorspellen$ZetelRaming[TeVoorspellen$Partij == "BIJ1" | TeVoorspellen$Partij == "Volt" | TeVoorspellen$Partij == "Code Oranje" |  TeVoorspellen$Partij == "JA21"] = TeVoorspellen$Percentage[TeVoorspellen$Partij == "BIJ1" | TeVoorspellen$Partij == "Volt" | TeVoorspellen$Partij == "Code Oranje" |  TeVoorspellen$Partij == "JA21"]

TeVoorspellen$ZetelRaming2[TeVoorspellen$Partij == "BIJ1" | TeVoorspellen$Partij == "Volt" | TeVoorspellen$Partij == "Code Oranje" |  TeVoorspellen$Partij == "JA21"] = TeVoorspellen$Percentage[TeVoorspellen$Partij == "BIJ1" | TeVoorspellen$Partij == "Volt" | TeVoorspellen$Partij == "Code Oranje" |  TeVoorspellen$Partij == "JA21"]

# ---- Resultaten presenteren ----
ZetelRaming = TeVoorspellen %>%
  filter(Dag == today()) %>% 
  mutate(afwijking = sum4$sigma) %>%
  arrange(ZetelRaming2) %>%  
  mutate(Partij = as_factor(Partij), ZetelRaming = (150/sum(ZetelRaming)) * ZetelRaming, 
    ZetelRaming2 = (150/sum(ZetelRaming2)) * ZetelRaming2,
    ZetelRaming = round(ZetelRaming, digits = 1), 
    ZetelRaming2 = round(ZetelRaming2, digits = 1))

ZetelRaming %>% ggplot(aes(x = Partij, y = ZetelRaming2, label = ZetelRaming2)) + 
  geom_col(fill = "#28ba98") + 
  geom_text(size = 4, position = position_stack(vjust = 0.5) ) + 
  coord_flip() +
  theme_hc() +
  theme(axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

