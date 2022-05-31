
# Unmeasured Antler Length
dantler_used_std = dantler[-which(is.na(dantler$AntlerLength)) , ]



dantler_used_std =dantler_used_std[,c("Id", 
                              "Year",
                              "Day",
                              "AntlerLength",
                              "AntlerType",
                              "AgeClass",
                              "Population")]
                              


# Fonctions de standardisation --------------------------------------------

DATE_REF = 60

std_antler = function(antler_length, date, pars, date_ref = DATE_REF) {
  a = pars[1]
  b = pars[2]
  (antler_length - (a * date + b))  + a * date_ref + b
}

# Définition des fonctions de modélisation de la longueur des bois --------

# fonction constante

constante = function(pars, x) {
  a = pars[1]
  
  a
}

# fonction linéaire

lineaire = function(pars, x = 0) {
  a = pars[1]
  b = pars[2]
  
  a * x + b
}

# fonction de seuil avec une pente et un plateau

one_slope = function(pars, x) {
  a = pars[1]
  b = pars[2]
  seuil = pars[3]
  
  ifelse(x < seuil, a * x + b, a * seuil + b)
}

# fonction de seuil avec deux pentes

two_slopes = function(pars, x) {
  a = pars[1]
  b = pars[2]
  seuil = pars[3]
  c = pars[4]
  OO_2 = (a - c) * seuil + b
  
  ifelse(test = x < seuil,
         yes = a * x + b,
         no = c * x + OO_2)
}



NLL = function(pars,
               ma_fonction,
               y = Y,
               x = X) {
  # Values predicted by the model
  sigma = pars[length(pars)]
  parametres_moyenne = pars[-length(pars)]
  Gpred = ma_fonction(parametres_moyenne, x)
  # Negative log-likelihood 
  -sum(dnorm(
    x = y,
    mean = Gpred,
    sd = sigma,
    log = TRUE
  ))
}

n = 1000
X = abs(rnorm(n))
Y = lineaire(c(2, 5, 1), X) + rnorm(length(X), sd = .5)



optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)

plan_experience = tibble(
  ma_fonction = c(constante, lineaire, one_slope, two_slopes),
  initial_pars = list(c(0, 1), c(1, 0, 1), c(1, 0, 1, 1), c(1, 0, 0, 2, 1))
)


purrr::pmap_dbl(plan_experience,get_AIC)

# Application aux données réelles -----------------------------------------

# CHIZE -------------------------------------------------------------------

# Premiere classe ---------------------------------------------------------

dantler_2 = subset(dantler_used_std, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "C" & AntlerLength > 1) # Les bois inf. à 1 sont considérer comme invariants

X = dantler_2$Day
Y = dantler_2$AntlerLength %>%
  log()

optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)


dantler_3 = mutate(dantler_2,
                       Antler_std = exp(std_antler(
                         log(AntlerLength), Day, optim_output$par
                       )))


dantler_1C = dantler_3[, c("Id", "Year", "Antler_std")]


# Classes supérieures -----------------------------------------------------


dantler_2 = subset(dantler_used_std, AntlerType == "BV"| is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "C") 

X = dantler_2$Day
Y = dantler_2$AntlerLength %>%
  log()

optim_output = optim(
  par = c(0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = constante,
      y = Y,
      x = X
    )
)




dantler_3 = mutate(dantler_2,
                       Antler_std = AntlerLength)


dantler_NC = dantler_3[, c("Id", "Year", "Antler_std")]

# Trois Fontaines ---------------------------------------------------------

# Premiere classe ---------------------------------------------------------

dantler_2 = subset(dantler_used_std,AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "TF" & AntlerLength > 0) 

X = dantler_2$Day
Y = dantler_2$AntlerLength %>%
  log()

optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)



dantler_3 = mutate(dantler_2,
                       Antler_std = exp(std_antler(
                         log(AntlerLength), Day, optim_output$par
                       )))


dantler_1TF = dantler_3[, c("Id", "Year", "Antler_std")]


# Classes supérieures -----------------------------------------------------


dantler_2 = subset(dantler_used_std, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "TF") 

X = dantler_2$Day
Y = dantler_2$AntlerLength %>%
  log()
optim_output = optim(
  par = c(1, 0, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = lineaire,
      y = Y,
      x = X
    )
)



dantler_3 = mutate(dantler_2,
                       Antler_std = exp(std_antler(
                         log(AntlerLength), Day, optim_output$par
                       )))

dantler_NTF = dantler_3[, c("Id", "Year", "Antler_std")]

dantler_std = rbind(dantler_1C,
                        dantler_NC,
                        dantler_1TF,
                        dantler_NTF)

dantler_complet <-  merge(dantler_used_std, dantler_std, by=c("Id", "Year"), all.x = TRUE)


# Traitement des cas Antler_Type==BD  & AntlerLength <= 1 --------------------

for (k in 1:nrow(dantler_complet)){
  if (!is.na(dantler_complet[k, "AntlerType"])){
    if (dantler_complet[k, "AntlerType"] == "BD"){ 
      dantler_complet[k, "Antler_std"] = dantler_complet[k, "AntlerLength"]
    }
  }
  if (dantler_complet[k, "AntlerLength"] <= 1){
    dantler_complet[k, "Antler_std"] = dantler_complet[k, "AntlerLength"]
  }
}

dantler_complet <- dantler_complet[,c("Id", "Year", "Antler_std")]
dantler <-  merge(dantler, dantler_complet, by=c("Id", "Year"), all.x = TRUE)


rm(k)
rm(n)
rm(X)
rm(Y)
rm(DATE_REF)
rm(optim_output)
rm(constante)
rm(lineaire)
rm(NLL)
rm(std_antler)
rm(dantler_1C)
rm(dantler_NC)
rm(dantler_1TF)
rm(dantler_NTF)
rm(dantler_std)
rm(dantler_complet)
rm(dantler_2)
rm(dantler_3)
rm(dantler_used_std)
rm(one_slope)
rm(two_slopes)


