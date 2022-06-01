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
# à procéder en deux étapes avec et sans RTL en variables explicatives.

# Afin de tenir en compte de la relation allométrique entre
#la taille des bois et la masse, la sélection de modèle sera en deux étapes:
#(i) la taille des bois et la masse sont des variables explicatives exclusives
#(ii) la taille des bois et la masse sont log-transformées et 
# sont obligatoirement intégrés aux différents modèles testés.

# Dans les analyses avec la log-transformation, les bois de taille nulle sont retirés


## Sélection de la population

dantler_NA = dantler %>% 
  subset(Age!=1) %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight, Antler_std, 
                RTL,
                AgeAccelLOO) 
vis_miss(dantler_NA)

dantler_NA = dantler_NA[complete.cases(dantler_NA$AgeAccelLOO), ]

vis_miss(dantler_NA)



# Sans la variable RTL ----------------------------------------------------

## Bois et masse exclusifs -------------------------------------------------

dantler_NA_without_RTL = dantler_NA %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight, Antler_std,
                AgeAccelLOO) %>% 
  na.omit()


reg_lm <- lm(AgeAccelLOO ~ Cohort + Population + 
                  Weight + Antler_std+
                  Antler_std:Cohort + Antler_std:Population + 
                  Weight:Cohort  + Weight:Population +
                  Cohort:Population
                ,  
                data=dantler_NA_without_RTL)


options(na.action = "na.fail")

fm <- reg_lm
ms <- dredge(fm)

head(ms)



## Bois et masse obligatoires -------------------------------------------------

# 0 ind. retirés: AntlerLength==0
dantler_NA[which(dantler_NA$Antler_std==0),]

dantler_NA_without_RTL = dantler_NA%>% 
  #subset(Antler_std!=0) %>% 
  mutate(Antler_std_log = log(Antler_std),
         Weight_log = log(Weight)) %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_log, Antler_std_log,
                AgeAccelLOO) %>% 
  na.omit()

reg_lm <- lm(AgeAccelLOO ~ Weight_log + Antler_std_log + Cohort + Population + 
                  Antler_std_log:Weight_log + Antler_std_log:Cohort + Antler_std_log:Population + 
                  Weight_log:Cohort  + Weight_log:Population +
                  Cohort:Population
                ,  
                data=dantler_NA_without_RTL)


options(na.action = "na.fail")

fm <- reg_lm
ms <- dredge(fm)

head(ms)
