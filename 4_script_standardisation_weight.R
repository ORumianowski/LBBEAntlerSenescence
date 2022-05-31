rm(list = ls()) # nettoyage de l'environnement de travail

source("1_utils_packages.R")
source("2_script_merge_dataset_telomere_epi.R")


# A faire avec données completes
                              
dantler_used_std =select(dantler, 
                             Id, 
                             Year,
                             Day,
                             Weight,
                             AgeClass,
                             Population) %>% 
  subset(AgeClass == "(0,1]")


ggplot(dantler_used_std,
       aes(x = Day,
           y = Weight,
           color = Population,
           shape = Year)) +
  geom_point() 

# Fonctions de standardisation --------------------------------------------

DATE_REF = 60

std_weight = function(weight, date, pars, date_ref = DATE_REF) {
  a = pars[1]
  b = pars[2]
  (weight_length - (a * date + b))  + a * date_ref + b
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




# Fonction d'optimisation par MV ------------------------------------------

n = 1000
X = abs(rnorm(n))
Y = two_slopes(c(-2, 2, 2, 13), X) + rnorm(length(X), sd = .5)



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

optim_output = optim(
  par = c(1, 1, 1, 1, 1),
  fn = function(p)
    NLL(
      p,
      ma_fonction = two_slopes,
      y = Y,
      x = X
    )
)


optim_output$par





# Application aux données réelles -----------------------------------------

# CHIZE -------------------------------------------------------------------

data_antler_chize = subset(dantler_used_std, Population == "C" )
X = data_antler_chize$Day
Y = data_antler_chize$Weight %>% 
  log()

ggplot(data_antler_chize,
       aes(x = Day,
           y = log(Weight),
           color = Year)) +
  geom_point() 

purrr::pmap_dbl(plan_experience, get_AIC)



ggplot(data_antler_chize,
       aes(x = X,
           y = Y)) +
  geom_point() +
  
  geom_abline(slope = optim_output$par[1],
              intercept = optim_output$par[2],
              colour = "red")+
  labs( title = "Chizé - 1A",
        x= "Julian Capture Date",
        y= "Antler Length (log)")


data_antler_3 = mutate(data_antler_2,
                       Antler_std = exp(std_antler(
                         log(AntlerLength), Day, optim_output$par
                       )))


data_antler_1C = data_antler_3[, c("Pop_Id", "Year", "Antler_std")]


# Classes supérieures -----------------------------------------------------


data_antler_2 = subset(data_antler_used_std, AntlerType == "BV"| is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "C") 

X = data_antler_2$Day
Y = data_antler_2$AntlerLength %>%
  log()

#purrr::pmap_dbl(plan_experience, get_AIC)

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


ggplot(data_antler_2,
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


data_antler_3 = mutate(data_antler_2,
                       Antler_std = AntlerLength)


data_antler_NC = data_antler_3[, c("Pop_Id", "Year", "Antler_std")]

# Trois Fontaines ---------------------------------------------------------

# Premiere classe ---------------------------------------------------------

data_antler_2 = subset(data_antler_used_std,AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass == "(0,1]" & Population == "TF") 

X = data_antler_2$Day
Y = data_antler_2$AntlerLength %>%
  log()

#purrr::pmap_dbl(plan_experience, get_AIC)

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

ggplot(data_antler_2,
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


data_antler_3 = mutate(data_antler_2,
                       Antler_std = exp(std_antler(
                         log(AntlerLength), Day, optim_output$par
                       )))


data_antler_1TF = data_antler_3[, c("Pop_Id", "Year", "Antler_std")]


# Classes supérieures -----------------------------------------------------


data_antler_2 = subset(data_antler_used_std, AntlerType == "BV" | is.na(AntlerType)) %>%
  subset(AgeClass != "(0,1]" & Population == "TF") 

X = data_antler_2$Day
Y = data_antler_2$AntlerLength %>%
  log()

#purrr::pmap_dbl(plan_experience, get_AIC)

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

ggplot(data_antler_2,
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


data_antler_3 = mutate(data_antler_2,
                       Antler_std = exp(std_antler(
                         log(AntlerLength), Day, optim_output$par
                       )))

data_antler_NTF = data_antler_3[, c("Pop_Id", "Year", "Antler_std")]

data_antler_std = rbind(data_antler_1C,
                        data_antler_NC,
                        data_antler_1TF,
                        data_antler_NTF)

data_antler_complet <-  merge(data_antler_used_std, data_antler_std, by=c("Pop_Id", "Year"), all.x = TRUE)


# Traitement des cas Antler_Type==BD  & AntlerLength <= 1 --------------------

for (k in 1:nrow(data_antler_complet)){
  if (!is.na(data_antler_complet[k, "AntlerType"])){
    if (data_antler_complet[k, "AntlerType"] == "BD"){ 
      data_antler_complet[k, "Antler_std"] = data_antler_complet[k, "AntlerLength"]
    }
  }
  if (data_antler_complet[k, "AntlerLength"] <= 1){
    data_antler_complet[k, "Antler_std"] = data_antler_complet[k, "AntlerLength"]
  }
}

data_antler_complet <- data_antler_complet[,c("Pop_Id", "Year", "Antler_std")]
data_antler <-  merge(data_antler, data_antler_complet, by=c("Pop_Id", "Year"), all.x = TRUE)