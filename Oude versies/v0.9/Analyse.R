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

setwd("D:/Documenten/dataproject/Peilingen") # Pas dit pad aan naar waar u de 
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

zetelpercentage = Peilingen %>% 
  select(Partij, Zetels, Percentage, Publicatie) %>% na.omit()

coefficients = lm(Percentage ~ Zetels , data = zetelpercentage)

coefficients = as.data.frame(summary(coefficients)$coefficients)

# Verhouding implementeren
Peilingen$Percentage[is.na(Peilingen$Percentage)] = 0

Peilingen$Percentage = if_else(Peilingen$Percentage == 0, 
  Peilingen$Zetels * coefficients[2, 1] +
  coefficients[1, 1], Peilingen$Percentage)
 
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
  mutate(PeilsterPlus = NULL, HuisPercentage = Percentage - Huiseffect, 
         Huiseffect = NULL, PeilingWeek = NULL)

# ---- Hoeveel voorspellender worden peilingen dichter bij de verkiezingen? ---- 
# Om dit uit te rekenen had ik per partij voor iedere peiling nodig wat het
# percentage bij de vorige peiling was. Ik koos om dit zonder for loop of 
# apply function, en daarom is de code wat omslachtig met het tijdelijk omzetten
# van NA in een negatief getal.

Peilingen = Peilingen %>% 
  arrange(Partij, Publicatie) %>% 
  mutate(PartijLag = Partij, 
    DagenVorige = lag(DagenTotVerkiezing) - DagenTotVerkiezing)

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

Peilingentot2002 = Peilingen %>%
  filter(!(as.numeric(Publicatie) > as.numeric(as_date("2012-1-1"))), Partij != "LPF")

Peilingen2012 = Peilingen %>% 
  filter((as.numeric(Publicatie) > 
    as.numeric(as_date("2012-1-1"))), Partij != "Code Oranje", 
    Partij != "JA21", Partij != "BIJ1", Partij != "Volt", Partij != "Overig")

# Begin objecten die nodig zijn om peilinggemiddeldes voor alle dagen voor alle 
# partijen te berekenen. Peilinggemiddeldes worden automatisch berekend voor de 
# laatste 13 peilingen met optimalisatie (Constrained quadratic programming), 
# en gebruikt de geschatte waardes van hoe voorspellend een peiling is gezien
# het aantal dagen tot de verkiezingen. De volgende vier zijn om uitslagen van
# de for loop in op te slaan.

Gemiddeldes2012 = tibble(Dagen = as.numeric(ymd("2012-1-1"):as.numeric(today())))

Gemiddeldes1965 =   Datums = tibble(Dagen = (as.numeric(ymd("1965-1-1") +
    1827):(as.numeric(ymd("2000-12-31")) + 1827)))


namen = c("Dagen")

ab = tibble(.rows = 0, w12 = 0, w11 = 0, w10 = 0, w9 = 0, w8 = 0, 
  w7 = 0, w6 = 0, w5 = 0, w4 = 0, w3 = 0, w2 = 0, w1 = 0, w0 = 0)

# Deze functie zorgt ervoor dat voor iedere dag de dichtsbijzijnde peiling wordt
# gevonden per partij, zolang die partij in de laatste 100 dagen in een peiling
# is verschenen.
dichstbij <- function(Nummer){
  b = Nummer - temp$Nummer
  b[b < 0] = 1e5
  if (min(b) < 100) {
    return(Nummer - min(b))
  }
  else{
    return(NA)
  }
}

# Dmat, bvec dvec en amat zijn de matrices die nodig zijn voor de funcite van 
# quadprog. Dmat zijn de constanten in de te optimaliseren formule, met dvec de  
# speciefieke waardes. Amat en bvec zijn samen de constraints.

Dmat = matrix(c(2 * sum3[["sigma"]] ^ 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 2 * sum3[["sigma"]] ^ 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 2 * sum3[["sigma"]] ^ 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 2 * sum3[["sigma"]] ^ 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 2 * sum3[["sigma"]] ^ 2, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 2 * sum3[["sigma"]] ^ 2, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 2 * sum3[["sigma"]] ^ 2, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 2 * sum3[["sigma"]] ^ 2, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 2 * sum3[["sigma"]] ^ 2, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 2 * sum3[["sigma"]] ^ 2, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2 * sum3[["sigma"]] ^ 2, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2 * sum3[["sigma"]] ^ 2, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2 * sum3[["sigma"]] ^ 2), 
  nrow = 13, ncol = 13, byrow = TRUE)

Amat = t(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), nrow = 14, ncol = 13, byrow = TRUE))

bvec = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# Einde van objecten die nodig zijn om peilinggemiddeldes voor alle dagen voor 
# alle partijen te berekenen. 


for (e in unique(Peilingen2012$Partij)) {
  temp = Peilingen2012 %>% 
    filter(Partij == e) %>%
    mutate(Nummer = as.numeric(Publicatie), 
      DagenTotVerkiezing = as.numeric(DagenTotVerkiezing)) %>% 
    select(Nummer, HuisPercentage, DagenTotVerkiezing)  %>%
    mutate(ZelfdeDag = lead(duplicated(Nummer)),
      lag0 = Nummer, 
      lag1 = lag(Nummer, n = 1L), lag2 = lag(Nummer, n = 2L), 
      lag3 = lag(Nummer, n = 3L), lag4 = lag(Nummer, n = 4L), 
      lag5 = lag(Nummer, n = 5L), lag6 = lag(Nummer, n = 6L), 
      lag7 = lag(Nummer, n = 7L), lag8 = lag(Nummer, n = 8L), 
      lag9 = lag(Nummer, n = 9L), lag10 = lag(Nummer, n = 10L), 
      lag11 = lag(Nummer, n = 11L), lag12 = lag(Nummer, n = 12L),
      p0 = HuisPercentage, 
      p1 = lag(HuisPercentage, n = 1L), p2 = lag(HuisPercentage, n = 2L), 
      p3 = lag(HuisPercentage, n = 3L), p4 = lag(HuisPercentage, n = 4L), 
      p5 = lag(HuisPercentage, n = 5L), p6 = lag(HuisPercentage, n = 6L), 
      p7 = lag(HuisPercentage, n = 7L), p8 = lag(HuisPercentage, n = 8L), 
      p9 = lag(HuisPercentage, n = 9L), p10 = lag(HuisPercentage, n = 10L), 
      p11 = lag(HuisPercentage, n = 11L), p12 = lag(HuisPercentage, n = 12L)
    ) %>%
    filter(ZelfdeDag == FALSE) %>%
    mutate(ZelfdeDag = NULL, HuisPercentage = NULL) %>% 
    na.omit()
  
  Datums = tibble(Dagen = as.numeric(ymd("2012-1-1"):as.numeric(today())))
  
  Datums$Nummer = sapply(X = Datums$Dagen, FUN = dichstbij)
  
  Datums = left_join(Datums, temp, by = "Nummer")
  
  Datums = Datums %>% 
    mutate(across(.cols = lag0:lag12, function(x) Dagen - as.numeric(x) +
        DagenTotVerkiezing)) %>%
    mutate(across(.cols = lag0:lag12, function(x) sum3$coefficients[1] + 
        sum3$coefficients[4] * x + sum3$coefficients[3] * log(x)))
  
  
  temp2 = Datums %>% na.omit()
  temp3 = ab
  
  
  for (i in 1:nrow(temp2)) { 
    dvec = c(temp2$lag0[i],temp2$lag1[i], temp2$lag2[i], temp2$lag3[i], 
      temp2$lag4[i], temp2$lag5[i], temp2$lag6[i], temp2$lag7[i], temp2$lag8[i],
      temp2$lag9[i], temp2$lag10[i], temp2$lag11[i], temp2$lag12[i])
    qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
    temp3[i, ] = as.list(qp$solution)
  }
  
  Datums =  temp2 %>% 
    mutate(yo = (temp3$w12 * p12 + temp3$w11 * p11 + temp3$w10 * p10 + 
        temp3$w9 * p9 + temp3$w8 * p8 + temp3$w7 * p7 + temp3$w7 * p7 + 
        temp3$w6 * p6 + temp3$w5 * p5 + temp3$w4 * p4 + temp3$w3 * p3 + 
        temp3$w2 * p2 + temp3$w1 * p1 + temp3$w0 * p0)) %>%
    select(yo, Dagen) %>% right_join(select(Datums, Dagen), by = "Dagen") %>% 
    arrange(Dagen) %>% select(yo)
  
  namen = c(namen, e)
  
  Gemiddeldes2012 = cbind(Gemiddeldes2012, Datums)
}

colnames(Gemiddeldes2012) = namen
Gemiddeldes2012 = Gemiddeldes2012 %>% 
  pivot_longer('50PLUS':VVD, names_to = "Partij") %>% 
  na.omit()

# En nog een keer!

namen = c("Dagen")

for (e in unique(Peilingentot2002$Partij)) {
  temp = Peilingentot2002 %>% 
    filter(Partij == e) %>%
    mutate(Nummer = as.numeric(Publicatie), 
      DagenTotVerkiezing = as.numeric(DagenTotVerkiezing)) %>% 
    select(Nummer, HuisPercentage, DagenTotVerkiezing)  %>%
    mutate(ZelfdeDag = lead(duplicated(Nummer)),
      lag0 = Nummer, 
      lag1 = lag(Nummer, n = 1L), lag2 = lag(Nummer, n = 2L), 
      lag3 = lag(Nummer, n = 3L), lag4 = lag(Nummer, n = 4L), 
      lag5 = lag(Nummer, n = 5L), lag6 = lag(Nummer, n = 6L), 
      lag7 = lag(Nummer, n = 7L), lag8 = lag(Nummer, n = 8L), 
      lag9 = lag(Nummer, n = 9L), lag10 = lag(Nummer, n = 10L), 
      lag11 = lag(Nummer, n = 11L), lag12 = lag(Nummer, n = 12L),
      p0 = HuisPercentage, 
      p1 = lag(HuisPercentage, n = 1L), p2 = lag(HuisPercentage, n = 2L), 
      p3 = lag(HuisPercentage, n = 3L), p4 = lag(HuisPercentage, n = 4L), 
      p5 = lag(HuisPercentage, n = 5L), p6 = lag(HuisPercentage, n = 6L), 
      p7 = lag(HuisPercentage, n = 7L), p8 = lag(HuisPercentage, n = 8L), 
      p9 = lag(HuisPercentage, n = 9L), p10 = lag(HuisPercentage, n = 10L), 
      p11 = lag(HuisPercentage, n = 11L), p12 = lag(HuisPercentage, n = 12L)
    ) %>%
    filter(ZelfdeDag == FALSE) %>%
    mutate(ZelfdeDag = NULL, HuisPercentage = NULL) %>% 
    na.omit()
  
  Datums = tibble(Dagen = (as.numeric(ymd("1965-1-1") + 
      1827):(as.numeric(ymd("2000-12-31")) + 1827)))
  
  Datums$Nummer = sapply(X = Datums$Dagen, FUN = dichstbij)
  
  Datums = left_join(Datums, temp, by = "Nummer")
  
  Datums = Datums %>% 
    mutate(across(.cols = lag0:lag12, function(x) Dagen - as.numeric(x) +
        DagenTotVerkiezing)) %>%
    mutate(across(.cols = lag0:lag12, function(x) sum3$coefficients[1] + 
        sum3$coefficients[4] * x + sum3$coefficients[3] * log(x)))
  
  
  temp2 = Datums %>% na.omit()
  temp3 = ab
  
  
  for (i in 1:nrow(temp2)) { 
    dvec = c(temp2$lag0[i],temp2$lag1[i], temp2$lag2[i], temp2$lag3[i], 
      temp2$lag4[i], temp2$lag5[i], temp2$lag6[i], temp2$lag7[i], temp2$lag8[i],
      temp2$lag9[i], temp2$lag10[i], temp2$lag11[i], temp2$lag12[i])
    
    qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
    
    temp3[i, ] = as.list(qp$solution)
  }
  
  Datums =  temp2 %>% 
    mutate(yo = (temp3$w12 * p12 + temp3$w11 * p11 + temp3$w10 * p10 + 
        temp3$w9 * p9 + temp3$w8 * p8 + temp3$w7 * p7 + temp3$w7 * p7 + 
        temp3$w6 * p6 + temp3$w5 * p5 + temp3$w4 * p4 + temp3$w3 * p3 + 
        temp3$w2 * p2 + temp3$w1 * p1 + temp3$w0 * p0)) %>%
    select(yo, Dagen) %>% right_join(select(Datums, Dagen), by = "Dagen") %>% 
    arrange(Dagen) %>% select(yo)
  
  namen = c(namen, e)
  
  Gemiddeldes1965 = cbind(Gemiddeldes1965, Datums)
}

colnames(Gemiddeldes1965) = namen

Gemiddeldes1965 = Gemiddeldes1965 %>% 
  mutate(Dagen = Dagen - 1827) %>% 
  pivot_longer(ARP:VVD, names_to = "Partij") %>% 
  na.omit()

Gemiddeldes = rbind(Gemiddeldes1965, Gemiddeldes2012) %>% 
  mutate(Publicatie = as_date(Dagen))

Gemiddeldes$Verkiezingsdag = case_when(Gemiddeldes$Publicatie > ymd("2017-03-15") ~ ymd("2021-03-15"),
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

Uitslagen = Uitslagen %>%
  mutate(Premier_GDP = GDPgroei * Premier, Regering_GDP = InRegering * GDPgroei,
    Premier_Werkloos = Premier * `Beroepsbevolking|Werkloosheidspercentage`,
    Regering_Werkloos = InRegering * `Beroepsbevolking|Werkloosheidspercentage`)

Covariates = tibble(Coefficient = 1:15, variabel = 
  c("Premier", "InRegering", "Uitgestapt", "VerkiezingenLijsttrekker", 
    "Minister", "MinisterPresident", "NaoorlogseVerkiezingsNummer", "Vrouw",
    "OoitMinister", "Premier_GDP", "Premier_Werkloos", "Regering_Werkloos",
    "Regering_GDP", "StemPercentageVorige", "GDPgroei"))

StanUitslag = Uitslagen %>% 
  select(c(Covariates$variabel), StemPercentage, Partij, Verkiezingsdag) %>% 
  na.omit()
 
StanUitslagen =  list(N = nrow(StanUitslag),
  K = ncol(StanUitslag) - 3, x = as.matrix(select(StanUitslag, 
    (!c("StemPercentage", "Partij", "Verkiezingsdag")))),
  y = c(StanUitslag$StemPercentage))

fit1 = stan(file = "model1.stan", data = StanUitslagen)

exfit = rstan::extract(fit1) %>%
  as.data.frame() %>%
  colMeans() %>%
  as.data.frame()

saveRDS(exfit1, file = "fitt1.RDS")

# exfit = readRDS("fitt1.RDS")

StanUitslag$Predicted =  exfit[1,] + 
      exfit[2,] * StanUitslag[, 1] + exfit[3,] * StanUitslag[, 2] + 
      exfit[4,] * StanUitslag[, 3] + exfit[5,] * StanUitslag[, 4] +
      exfit[6,] * StanUitslag[, 5] + exfit[7,] * StanUitslag[, 6] + 
      exfit[8,] * StanUitslag[, 7] + exfit[9,] * StanUitslag[, 8] + 
      exfit[10,] * StanUitslag[, 9] + exfit[11,] * StanUitslag[, 10] + 
      exfit[12,] * StanUitslag[, 11] + exfit[13,] * StanUitslag[, 12] +
      exfit[14,] * StanUitslag[, 13] + exfit[15,] * StanUitslag[, 14] + 
      exfit[16,] * StanUitslag[, 15]
     

StanUitslag = mutate(StanUitslag, Predicted = Predicted)

StanUitslag = as.data.frame(StanUitslag)
  
Gemiddeldes = Gemiddeldes %>% 
  mutate(
    DagenTotVerkiezingen = as.numeric(Verkiezingsdag) - Dagen, Dagen = NULL) %>% 
  left_join(StanUitslag, by = c("Verkiezingsdag", "Partij"))

Gemiddeldes2 = Gemiddeldes %>%
  mutate(PeilingVerschil = value - StemPercentageVorige) %>% 
  select(value, DagenTotVerkiezingen, Predicted, StemPercentage) %>%
  na.omit()

Gemiddeldes2 = Gemiddeldes2 %>% 
  mutate(PeilingDagen = value * DagenTotVerkiezingen,
    Predicted = Predicted$Premier,
  PredictedDagen = Predicted * DagenTotVerkiezingen)


# StanLijst = list(N = nrow(Gemiddeldes),
#   PeilingVerschil = c(Gemiddeldes$value),
#   PeilingDagen = c(Gemiddeldes$PeilingDagen),
#   Predicted = c(Gemiddeldes$Predicted$Premier),
#   PredictedDagen = c(Gemiddeldes$PredictedDagen$Premier),
#   Verschil = c(Gemiddeldes$StemPercentage))
# 
# fit2 = stan(file = "ZetelRaming.stan", data = StanLijst)
# 
# exfit2 = rstan::extract(fit2) %>% 
#   as.data.frame() %>% 
#   colMeans() %>% 
#   as.data.frame()

sum4 = lm(StemPercentage ~ value + Predicted, data = Gemiddeldes2) %>% summary()

TeVoorspellenUitslagen = Uitslagen %>% 
  select(c(Covariates$variabel), StemPercentage, Partij, Verkiezingsdag) %>%
  filter(Verkiezingsdag == ymd("2021-3-15"))

TeVoorspellenUitslagen$StemPercentageVorige[
  TeVoorspellenUitslagen$Partij == "JA21" | 
    TeVoorspellenUitslagen$Partij == "Code Oranje" | 
    TeVoorspellenUitslagen$Partij == "BIJ1" |  
    TeVoorspellenUitslagen$Partij == "Volt" ] = 0

TeVoorspellenUitslagen$Predicted =  exfit[1,] + 
  exfit[2,] * TeVoorspellenUitslagen[, 1] + exfit[3,] * TeVoorspellenUitslagen[, 2] + 
  exfit[4,] * TeVoorspellenUitslagen[, 3] + exfit[5,] * TeVoorspellenUitslagen[, 4] +
  exfit[6,] * TeVoorspellenUitslagen[, 5] + exfit[7,] * TeVoorspellenUitslagen[, 6] + 
  exfit[8,] * TeVoorspellenUitslagen[, 7] + exfit[9,] * TeVoorspellenUitslagen[, 8] + 
  exfit[10,] * TeVoorspellenUitslagen[, 9] + exfit[11,] * TeVoorspellenUitslagen[, 10] + 
  exfit[12,] * TeVoorspellenUitslagen[, 11] + exfit[13,] * TeVoorspellenUitslagen[, 12] +
  exfit[14,] * TeVoorspellenUitslagen[, 13] + exfit[15,] * TeVoorspellenUitslagen[, 14] +
  exfit[16,] * TeVoorspellenUitslagen[, 15]

TeVoorspellen = Gemiddeldes %>% 
  filter(Verkiezingsdag == ymd("2021-3-15")) %>% 
  select(Partij, Dag = Publicatie, value, Verkiezingsdag) %>% 
  left_join(TeVoorspellenUitslagen, by = c("Partij", "Verkiezingsdag")) %>% 
  select(Partij, Dag, value, Predicted) %>% 
  mutate(ZetelramingPercentage = sum4$coefficients[1] + value * sum4$coefficients[2] + 
      Predicted  * sum4$coefficients[3], ZetelRaming = ZetelramingPercentage / coefficients[2, 1] -  coefficients[1, 1] )

# ---- Resultaten presenteren ----
ZetelRaming = TeVoorspellen %>%
  filter(Dag == today()) %>% 
  mutate(afwijking = sum4$sigma) %>%
  arrange(ZetelRaming$Premier) %>%  
  mutate(Partij = as_factor(Partij), ZetelRaming = round(ZetelRaming$Premier, digits = 1))

ZetelRaming %>% ggplot(aes(x = Partij, y = ZetelRaming, label = ZetelRaming)) + 
  geom_col(fill = "#28ba98") + 
  geom_text(size = 4, position = position_stack(vjust = 0.5) ) + 
  coord_flip() +
  theme_hc() +
  theme(axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

Overtijd = TeVoorspellen %>% na.omit() %>% ggplot(aes(x = Dag, y = ZetelRaming)) +
  geom_lin

