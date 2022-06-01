rm(list = ls()) # nettoyage de l'environnement de travail

setwd("C:/Users/odinr/Desktop/Stage LBBE MMIP/LBBEAntlerSenescence")

source("1_utils_packages.R")
source("2_script_merge_dataset_telomere_epi.R")
source("3_script_standardisation_antler.R")
source("5_script_epi_metrics.R")

vis_miss(dantler)

###
# Ce script a  pour objectif d'appliquer des modèles linéaires avec:
# Population: les individus avec un âge sup. à 1an
# L'analyse de la localisation des données manquantes nous incite
# à procéder en deux étapes avec et sans RTL en variables explicatives


dantler_Ad = dantler %>% 
  subset(Age>1) %>% 
  dplyr::select(Id, Year, Day, Age,
                               Cohort, Population, 
                               Weight_log, Antler_std, 
                               RTL,
                               EpiSimpleDiff)

## Prepation pour le passage au log
# Potentiels ind.à  retirés: AntlerLength==0
dantler_Ad[which(dantler_Ad$Antler_std==0),] # aucun

dantler_Ad = dantler_Ad %>% 
  mutate(Antler_std_log = log(Antler_std))

vis_miss(dantler_Ad)

# avec RTL

dantler_Ad_with_RTL = dantler_Ad %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_log, Antler_std_log, Antler_std,
                RTL,
                EpiSimpleDiff) %>% 
  na.omit()


reg_lm_1A <- lm(EpiSimpleDiff ~ Cohort + Population + 
                Weight_log + Antler_std_log + 
                RTL,  
                data=dantler_Ad_with_RTL)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

par(mar = c(3,5,6,4))
plot(ms1A, labAsExpr = TRUE)

# sans RTL

dantler_Ad_without_RTL = dantler_Ad %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_log, Antler_std_log, Antler_std,
                EpiSimpleDiff) %>% 
  na.omit()


reg_lm_1A <- lm(EpiSimpleDiff ~ Cohort + Population + 
                  Weight_log + Antler_std_log,  
                data=dantler_Ad_without_RTL)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

par(mar = c(3,5,6,4))
plot(ms1A, labAsExpr = TRUE)
