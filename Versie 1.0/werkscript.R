Peilingen = Peilingen %>% filter(StemPercentage >= 5.2)
Overview = list()

for (o in 2:20) {
max = o  
  
Dmat = diag(rep(2 * sum3[["sigma"]] ^ 2, max))

Amat = cbind(rep(1, max), diag(rep(1, max)))

bvec = c(1, rep(0, max))

Gemiddeldes = tibble(today(), 2.1, "Partij", 
  .rows = 0)
colnames(Gemiddeldes) = c("Publicatie", "Percentage", "Partij")

for (e in unique(Peilingen$Partij)) {
  peilset = Peilingen %>% 
    filter(Partij == e) %>% 
    select(dag = Publicatie, 
      dagentotverkiezing = DagenTotVerkiezing,
      percentages = HuisPercentage, Partij) %>%
    mutate(ZelfdeDag = lead(duplicated(dag))) %>% 
    mutate(dag = as.numeric(dag),
      dagentotverkiezing = as.numeric(dagentotverkiezing))

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


Overview[[max]] = Gemiddeldes %>% 
  left_join(Uitslagen, by = c("Partij", "Verkiezingsdag")) %>% 
  mutate(afwijking = sqrt((Percentage - StemPercentage) ^ 2), samenvatting = mean(afwijking, na.rm = TRUE)) %>%
  select(Partij, Publicatie, afwijking, samenvatting)

}

saveRDS(Overview, "Overview.RDS")