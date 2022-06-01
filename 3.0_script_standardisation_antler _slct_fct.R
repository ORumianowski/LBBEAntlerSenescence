#Script utilisé pour sélection la meilleure fonction pour 
#standardiser les longueurs de bois

rm(list = ls()) # nettoyage de l'environnement de travail

setwd("C:/Users/odinr/Desktop/Stage LBBE MMIP/LBBEAntlerSenescence")

source("1_utils_packages.R")
source("2_script_merge_dataset_telomere_epi.R")


# Unmeasured Antler Length
dantler_used_std = dantler[-which(is.na(dantler$AntlerLength)) , ]



dantler_used_std =dantler_used_std[,c("Id", 
                                      "Year",
                                      "Day",
                                      "AntlerLength",
                                      "AntlerType",
                                      "AgeClass",
                                      "Population")]


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


# Fonction d'optimisation par MV ------------------------------------------

n = 1000
X = abs(rnorm(n))
Y = lineaire(c(2, 5, 1), X) + rnorm(length(X), sd = .5)


plan_experience = tibble(
  ma_fonction = c(constante, lineaire, one_slope, two_slopes),
  initial_pars = list(c(0, 1), c(1, 0, 1), c(1, 0, 1, 1), c(1, 0, 0, 2, 1))
)


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

get_AIC = function(initial_pars,
                   ma_fonction,
                   y = Y,
                   x = X) {
  neg_log_lik = optim(
    par = initial_pars,
    fn = function(p)
      NLL(
        p,
        ma_fonction = ma_fonction,
        y = Y,
        x = X
      ),
    hessian = TRUE
  )$value
  2 * neg_log_lik + 2 * length(initial_pars)
}

purrr::pmap_dbl(plan_experience,get_AIC)





# Application aux données réelles -----------------------------------------

# CHIZE -------------------------------------------------------------------

# Premiere classe ---------------------------------------------------------

dantler_2 = subset(dantler_used_std, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "C" & AntlerLength > 0) # Les bois inf. à 1 sont considérer comme invariants

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

AIC_1C = purrr::pmap_dbl(plan_experience, get_AIC)

plot_1C = ggplot(dantler_2,
       aes(x = X,
           y = Y,
           color = AntlerType)) +
  geom_point() +
  
  geom_abline(slope = optim_output$par[1],
              intercept = optim_output$par[2],
              colour = "red")+
  labs( title = "Chizé - 1A",
        x= "Julian Capture Date",
        y= "Antler Length (log)")



# Classes supérieures -----------------------------------------------------


dantler_2 = subset(dantler_used_std, AntlerType == "BV"| is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "C"& AntlerLength > 0) 

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

AIC_NC = purrr::pmap_dbl(plan_experience, get_AIC)

plot_NC = ggplot(dantler_2,
       aes(x = X,
           y = Y,
           color = AntlerType)) +
  geom_point() +
  
  geom_abline(slope = 0,
              intercept = optim_output$par[1],
              colour = "red")  +
  labs( title = "Chizé - +1A",
        x= "Julian Capture Date",
        y= "Antler Length (log)")


# Trois Fontaines ---------------------------------------------------------

# Premiere classe ---------------------------------------------------------

dantler_2 = subset(dantler_used_std,AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "TF"& AntlerLength > 0) 

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

AIC_1TF = purrr::pmap_dbl(plan_experience, get_AIC)

plot_1TF = ggplot(dantler_2,
       aes(x = X,
           y = Y,
           color = AntlerType)) +
  geom_point() +
  
  geom_abline(slope = optim_output$par[1],
              intercept = optim_output$par[2],
              colour = "red")+
  labs( title = "Trois-Fontaines - 1A",
        x= "Julian Capture Date",
        y= "Antler Length (log)")


# Classes supérieures -----------------------------------------------------


dantler_2 = subset(dantler_used_std, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "TF"& AntlerLength > 0) 

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

AIC_NTF = purrr::pmap_dbl(plan_experience, get_AIC)


plot_NTF = ggplot(dantler_2,
       aes(x = X,
           y = Y,
           color = AntlerType)) +
  geom_point() +
  
  geom_abline(slope = optim_output$par[1],
              intercept = optim_output$par[2],
              colour = "red")+
  labs( title = "Trois-Fontaines - +1A",
        x= "Julian Capture Date",
        y= "Antler Length (log)")


AIC_1C
AIC_NC
AIC_1TF
AIC_NTF


plot_1C
plot_NC
plot_1TF
plot_NTF

#question: interaction indésirable entre les plots

